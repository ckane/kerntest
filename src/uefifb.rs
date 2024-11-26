use crate::driver::{Driver, DriverBus, DriverEntry, DriverError, DRIVERS};
use crate::framebuffer::UnsafeFrameBuffer;
use crate::memdrv::PatTypes;
use crate::physmap::PhysMapper;
use crate::GKARG;

use core::fmt::Write;
use embedded_graphics::{
    draw_target::DrawTarget,
    geometry::{OriginDimensions, Point, Size},
    mono_font::{ascii::FONT_6X10, MonoTextStyle},
    pixelcolor::Rgb888,
    prelude::*,
    primitives::rectangle::Rectangle,
    text::Text,
    Pixel,
};

use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::any::Any;
use core::sync::atomic::{AtomicPtr, Ordering};
use linkme::distributed_slice;
use log::info;

#[derive(Debug)]
struct UefiFrameBuffer {
    fb: AtomicPtr<u32>,
    fbsize: usize,
    stride: u32,
    fx: u32,
    fy: u32,
    txtcur_x: usize,
    txtcur_y: usize,
    console_buffer: Vec<String>,
    buffer_limit: usize,
    draw_buffer: Vec<u32>,
}

impl TryFrom<&mut UnsafeFrameBuffer> for UefiFrameBuffer {
    type Error = DriverError;

    fn try_from(ufb: &mut UnsafeFrameBuffer) -> core::result::Result<Self, Self::Error> {
        if let Ok(remap_fb) = unsafe {
            PhysMapper::map_phys::<u32>(
                ufb.get_fb() as usize,
                ufb.fbsize(),
                PatTypes::WriteCombining,
            )
        } {
            let mut old_buffer = unsafe {
                core::slice::from_raw_parts_mut(ufb.get_fb() as *mut u32, ufb.fbsize() >> 2)
            };
            Ok(Self {
                fb: AtomicPtr::new(remap_fb.get_vptr()),
                fbsize: ufb.fbsize(),
                fx: ufb.size().width,
                fy: ufb.size().height,
                stride: ufb.get_stride() as u32,
                txtcur_x: ufb.get_txtcur().0,
                txtcur_y: ufb.get_txtcur().1,
                console_buffer: vec![],
                buffer_limit: 2 * ufb.size().height as usize / 10,
                draw_buffer: Vec::from(old_buffer), // Creates a new local (cached) mem buffer
            })
        } else {
            Err(Self::Error::Initialization {
                reason: String::from("Failed mapping framebuffer"),
            })?
        }
    }
}

impl OriginDimensions for UefiFrameBuffer {
    fn size(&self) -> embedded_graphics::geometry::Size {
        embedded_graphics::geometry::Size::new(self.fx as u32, self.fy as u32)
    }
}

impl DrawTarget for UefiFrameBuffer {
    type Error = core::convert::Infallible;
    type Color = embedded_graphics::pixelcolor::Rgb888;

    fn draw_iter<I>(&mut self, pixels: I) -> core::result::Result<(), Self::Error>
    where
        I: IntoIterator<Item = Pixel<Self::Color>>,
    {
        for Pixel(Point { x: px, y: py }, color) in pixels.into_iter() {
            let x = px;
            let y = py;
            if (x < self.fx as i32) && (y < self.fy as i32) {
                /* Calculate offset into framebuffer */
                let offset = (y as u32 * (self.stride)) + (x as u32);
                self.draw_buffer[offset as usize] = 0xff000000
                    + ((color.r() as u32) << 16)
                    + ((color.g() as u32) << 8)
                    + (color.b() as u32);
            }
        }
        Ok(())
    }
}

impl Write for UefiFrameBuffer {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        /* TODO - smarter about breaking too-long strings. */
        for i in 0..s.len() {
            let text_style = MonoTextStyle::new(&FONT_6X10, Rgb888::YELLOW);
            if (self.txtcur_y * 10) + 20 < self.fy as usize {
                if &s[i..(i + 1)] == "\n" {
                    self.txtcur_y += 1;
                    self.txtcur_x = 0;

                    // Fill background of new line with black
                    //self.fill_solid(&Rectangle::new(Point::new(0i32, (self.txtcur_y * 10) as i32),
                    //                                Size::new(self.fx as u32, (self.txtcur_y * 10 + 10) as u32)), Rgb888::BLACK);
                    self.console_buffer.push(String::from(""));
                    if self.console_buffer.len() > self.buffer_limit {
                        self.console_buffer.remove(0);
                    }
                } else {
                    if (self.txtcur_x * 7) + 7 >= self.fx as usize {
                        self.txtcur_y += 1;
                        self.txtcur_x = 0;

                        // Fill background of new line with black
                        //self.fill_solid(&Rectangle::new(Point::new(0i32, (self.txtcur_y * 10) as i32),
                        //                                Size::new(self.fx as u32, (self.txtcur_y * 10 + 10) as u32)), Rgb888::BLACK);

                        self.console_buffer.push(String::from(""));
                        if self.console_buffer.len() > self.buffer_limit {
                            self.console_buffer.remove(0);
                        }
                    }
                    Text::new(
                        &s[i..(i + 1)],
                        Point::new((self.txtcur_x * 7) as i32, (self.txtcur_y * 10 + 9) as i32),
                        text_style,
                    )
                    .draw(self)
                    .unwrap();

                    // Advance the cursor
                    self.txtcur_x += 1;

                    // Put the character into the buffer
                    let c = s[i..(i + 1)].chars().next().unwrap().clone();
                    let last_row = self.console_buffer.len() - 1;
                    self.console_buffer.get_mut(last_row).unwrap().push(c);
                }
            } else {
                // TODO: Scroll the buffer up one row
                self.scroll_one();
                self.txtcur_y -= 1;
                self.txtcur_x = 0;
                // Fill background of new line with black
                //self.fill_solid(&Rectangle::new(Point::new(0i32, (self.txtcur_y * 10) as i32),
                //                                Size::new(self.fx as u32, (self.txtcur_y * 10 + 10) as u32)), Rgb888::BLACK);
                self.console_buffer.push(String::from(""));
                if self.console_buffer.len() > self.buffer_limit {
                    self.console_buffer.remove(0);
                }
                return self.write_str(s);
            }
        }
        // Copy the local mem buffer to the HW framebuffer (page flip)
        let fb = unsafe {
            core::slice::from_raw_parts_mut(self.fb.load(Ordering::Relaxed), self.fbsize / 4)
        };
        fb.clone_from_slice(self.draw_buffer.as_slice());
        Ok(())
    }
}

impl Driver for UefiFrameBuffer {
    fn new(bus: &mut dyn DriverBus) -> Result<alloc::sync::Arc<dyn Driver>, DriverError>
    where
        Self: Sized,
    {
        let mut karg = unsafe { (*GKARG).clone() };
        let s = Arc::new(Self::try_from(karg.get_fb())?);
        crate::klog::KernLogger::set_log_output(Arc::as_ptr(&s) as *mut UefiFrameBuffer);
        info!("Initialized UEFIFB @ {:?}", s.fb);
        Ok(s)
    }

    fn set(
        &self,
        s: &str,
        val: Arc<dyn Any + Send + Sync>,
    ) -> core::result::Result<(), crate::driver::Error> {
        Err(DriverError::AssetNotProvided { name: s.into() })
    }

    fn get(
        &self,
        s: &str,
    ) -> core::result::Result<Arc<dyn Any + Sync + Send>, crate::driver::Error> {
        match s {
            "framebuffer" => Ok(Arc::new(self.fb.load(Ordering::Relaxed) as usize)),
            _ => Err(crate::driver::Error::AssetNotProvided {
                name: String::from(s),
            }),
        }
    }
}

impl UefiFrameBuffer {
    fn scroll_one(&mut self) {
        let src_range = Rectangle {
            top_left: Point { x: 0, y: 10 },
            size: Size {
                width: self.fx as u32,
                height: self.fy as u32 - 10,
            },
        };
        self.s2s_blit(src_range, Point { x: 0, y: 0 });
        let blk_offset = (self.fy - 20) * self.stride;
        self.draw_buffer
            .get_mut(blk_offset as usize..((self.fy * self.stride) as usize))
            .unwrap()
            .fill(0);
        //let fb = unsafe { core::slice::from_raw_parts_mut(self.fb.load(Ordering::Relaxed), self.fbsize / 4) };
        //fb.clone_from_slice(self.draw_buffer.as_slice());
    }

    pub fn s2s_blit(&mut self, src_rect: Rectangle, dst_top_left: Point) {
        let tly = src_rect.top_left.y as usize;
        let maxw = src_rect.size.width as usize;
        let maxh = src_rect.size.height as usize;
        let tlx = src_rect.top_left.x as usize;
        for yoffs in 0..(src_rect.size.height as usize) {
            let ypos = if src_rect.top_left.y < dst_top_left.y {
                maxh - yoffs - 1
            } else {
                yoffs
            };
            let old_offset = ((tly + ypos) * (self.stride as usize)) + (tlx);
            let new_offset = ((dst_top_left.y as usize + ypos) * (self.stride as usize))
                + (dst_top_left.x as usize);
            self.draw_buffer
                .copy_within(old_offset..(old_offset + maxw), new_offset);
        }
    }
}

#[distributed_slice(DRIVERS)]
pub static UEFIFB_DRIVER_RECORD: DriverEntry = DriverEntry {
    name: "uefifb",
    req: &[],
    provides: &["framebuffer", "log"],
    ctor: UefiFrameBuffer::new,
};
