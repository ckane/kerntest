use crate::acpi::{LocalApicData, NmiInfo};
use crate::driver::{Driver, DriverBus, DriverEntry, DriverError, DRIVERS};
use crate::memdrv::PatTypes;
use crate::physmap::PhysMapper;

use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::any::Any;
use core::arch::asm;
use core::sync::atomic::{AtomicPtr, Ordering};
use linkme::distributed_slice;
use log::info;
use snafu::prelude::*;

#[derive(Debug)]
pub struct LocalApicDriver {
    lapic_phys: usize,
    nmi_lines: Vec<NmiInfo>,
    lapic_vaddr: AtomicPtr<u32>,
}

impl Driver for LocalApicDriver {
    fn new(bus: &mut dyn DriverBus) -> core::result::Result<Arc<dyn Driver>, crate::driver::Error> {
        Ok(Arc::new(Self::init(bus)?))
    }

    fn set(&self, s: &str, val: Arc<dyn Any + Send + Sync>) -> Result<(), crate::driver::Error> {
        match s {
            // Handle an EOI
            "timerint" => {
                //self.set_reg_addr(0x380, 0x1000000);
                Ok(self.set_reg_addr(0xb0, 0))
            }
            _ => Err(DriverError::AssetNotProvided { name: s.into() }),
        }
    }

    fn get(
        &self,
        s: &str,
    ) -> core::result::Result<Arc<dyn Any + Sync + Send>, crate::driver::Error> {
        Err(DriverError::AssetNotProvided { name: s.into() })
    }
}

impl LocalApicDriver {
    fn init(bus: &dyn DriverBus) -> core::result::Result<Self, crate::driver::Error> {
        if let Ok(lapic_data) = bus.get("lapic")?.downcast::<LocalApicData>() {
            let lapic_remap = unsafe {
                PhysMapper::map_phys::<u32>(
                    lapic_data.get_phys_addr(),
                    0x1000,
                    PatTypes::Uncacheable,
                )
            }
            .map_err(|e| crate::driver::Error::Initialization {
                reason: format!("{:?}", e),
            })?;
            // Construct a new Self and populate it with the LAPIC data
            let s = Self {
                lapic_phys: lapic_data.get_phys_addr(),
                nmi_lines: lapic_data.get_nmi_lines().clone(),
                lapic_vaddr: AtomicPtr::new(lapic_remap.get_vptr()),
            };

            info!(
                "Found LAPIC @ {:#018x}, nmi lines: {:?}",
                s.lapic_phys, s.nmi_lines
            );
            info!("Mapped LAPIC @ {:?}", s.lapic_vaddr);

            s.set_reg_addr(0x80, 0);

            // Set DFR
            s.set_reg_addr(0xe0, 0xffffffff);

            // OR the lower bit of LDR
            let mut ldr = s.get_reg_addr(0xd0);
            ldr |= 1;
            s.set_reg_addr(0xd0, ldr);

            // Disable LVT entries
            s.set_reg_addr(0x320, 0x10000);
            s.set_reg_addr(0x340, 4 << 8);
            s.set_reg_addr(0x350, 0x10000);
            s.set_reg_addr(0x360, 0x10000);
            s.set_reg_addr(0x80, 0);

            // Enable the LAPIC (if it isn't already) via MSR 0x1b
            unsafe { asm!("mov ecx, 0x1b", "rdmsr", "bts eax, 11", "wrmsr",) };

            // Set Spurious Interrupt LVT
            s.set_reg_addr(0xf0, 0x1ff);

            // Test Timer countdown
            s.set_reg_addr(0x380, 0xffffffff);
            s.set_reg_addr(0x320, 0x20);
            info!("Testing LAPIC Timer");
            s.set_reg_addr(0x320, 0x10000);
            let new_count = s.get_reg_addr(0x390);
            info!("Countdown at: {:#010x}", new_count);

            // Set the Timer LVT
            s.set_reg_addr(0x380, 0x1000000);
            s.set_reg_addr(0x320, 0x20020);
            s.set_reg_addr(0x3e0, 3);

            info!("LAPIC id {:#10x}", s.get_reg_addr(0x20));
            info!("LAPIC version {:#10x}", s.get_reg_addr(0x30));
            info!("LAPIC LDR {:#10x}", s.get_reg_addr(0xd0));
            info!("LAPIC DFR {:#10x}", s.get_reg_addr(0xe0));
            info!("LAPIC SIVR {:#10x}", s.get_reg_addr(0xf0));
            Ok(s)
        } else {
            Err(crate::driver::Error::Initialization {
                reason: "Failed to get lapic from bus".into(),
            })
        }
    }

    /// Get a register value, given the byte offset
    fn get_reg_addr(&self, index: usize) -> u32 {
        let lapic_slice = unsafe {
            core::slice::from_raw_parts_mut(
                self.lapic_vaddr.load(Ordering::Relaxed) as *mut u32,
                0x400,
            )
        };
        lapic_slice[index >> 2]
    }

    /// Set a register value, given the byte offset
    fn set_reg_addr(&self, index: usize, val: u32) {
        let lapic_slice = unsafe {
            core::slice::from_raw_parts_mut(
                self.lapic_vaddr.load(Ordering::Relaxed) as *mut u32,
                0x400,
            )
        };
        lapic_slice[index >> 2] = val;
    }
}

#[distributed_slice(DRIVERS)]
pub static LAPIC_DRIVER_RECORD: DriverEntry = DriverEntry {
    name: "lapic",
    req: &["lapic"],
    provides: &["timerint"],
    ctor: LocalApicDriver::new,
};
