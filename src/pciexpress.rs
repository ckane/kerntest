use crate::acpi::{AcpiDriverData, EcamSegment};
use crate::driver::{DRIVERS, Driver, DriverBus, DriverEntry, DriverError};

use alloc::sync::Arc;
use alloc::string::String;
use core::any::Any;
use core::ops::Deref;
use linkme::distributed_slice;
use log::info;
use snafu::prelude::*;

#[repr(C, packed(1))]
#[derive(Copy, Clone, Debug)]
struct PciDev {
    vendor: u16,
    devid: u16,
    command: u16,
    status: u16,
    rev: u8,
    prog_if_byte: u8,
    sub_class: u8,
    class: u8,
    cache_line_size: u8,
    latency_timer: u8,
    header_type: u8,
    bist: u8,
}

impl PciDev {
    fn get_vendor(&self) -> u16 {
        let local = self.vendor;
        local
    }
    fn get_devid(&self) -> u16 {
        let local = self.devid;
        local
    }
}

struct PciExpressDriver;

impl PciExpressDriver {
    fn calc_offset(base: usize, bus: u8, dev: u8, func: u8) -> usize {
        base + ((bus as usize) << 20) + (((dev & 0x1f) as usize) << 15) + (((func & 0x7) as usize) << 12)
    }
    fn walk_segment_group(seg: EcamSegment) {
        let mut count = 0;
        for bus in seg.bus_start()..=seg.bus_end() {
            for dev in 0..32 {
                for func in 0..8 {
                    let pcie_offs = Self::calc_offset(seg.phys_addr(), bus, dev, func);
                    let pcie_dev = unsafe { (pcie_offs as *mut PciDev).as_mut() };
                    if let Some(pd) = pcie_dev {
                        if pd.vendor != 0xffff && pd.vendor != 0 {
                            info!("pci({:02x},{:02x},{:01x}): {:#018x} {:#06x} {:#06x} {:#04x} {:#04x} {:#04x} {:#04x} {:#04x}",
                                bus, dev, func,
                                pcie_offs, pd.get_vendor(), pd.get_devid(), pd.rev, pd.class, pd.sub_class, pd.header_type,
                                pd.prog_if_byte
                            );
                            count += 1;
                            if count > 40 {
                                return
                            }
                        }
                    }
                }
            }
        }
    }
}

impl Driver for PciExpressDriver {
    fn new(bus: &mut dyn DriverBus) -> Result<alloc::sync::Arc<dyn Driver>, DriverError> where Self: Sized {
        if let Ok(AcpiDriverData::EcamPointer(segs)) = bus.get("pcie_ecam")?.downcast::<AcpiDriverData>().map(|x| x.deref().clone()) {
            for seg in segs {
                Self::walk_segment_group(seg);
            }
            Ok(Arc::new(Self))
        } else {
            Err(DriverError::Initialization { reason: String::from("Invalid Data returned for ECAM") })
        }
    }

    fn set(&self, s: &str, val: Arc<dyn Any + Send + Sync>) -> core::result::Result<(), crate::driver::Error> {
        Err(DriverError::AssetNotProvided { name: s.into() })
    }

    fn get(&self, s: &str) -> Result<Arc<dyn Any + Sync + Send>, DriverError> {
        Err(DriverError::AssetNotProvided { name: String::from(s) })
    }
}

#[distributed_slice(DRIVERS)]
pub static PCIEXPRESS_DRIVER_RECORD: DriverEntry = DriverEntry {
    name: "pciexpress",
    req: &["pcie_ecam"],
    provides: &[],
    ctor: PciExpressDriver::new
};
