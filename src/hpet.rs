use crate::driver::{DRIVERS, Driver, DriverBus, DriverEntry, DriverError};
use crate::memdrv::PatTypes;
use crate::physmap::PhysMapper;

use acpi::{AcpiError, hpet::HpetInfo};
use alloc::string::String;
use alloc::sync::Arc;
use core::any::Any;
use core::sync::atomic::AtomicPtr;
use linkme::distributed_slice;
use log::info;

struct HpetDriver {
    mmio: AtomicPtr<u64>,
    num_timers: u64,
}

impl Driver for HpetDriver {
    fn new(bus: &mut dyn DriverBus) -> Result<alloc::sync::Arc<dyn Driver>, DriverError> where Self: Sized {
        if let Ok(hpet_info) = bus.get("hpet_info")?.downcast::<HpetInfo>() {
            let hpet_base_addr = hpet_info.base_address;
            if let Ok(hpet_remap) = unsafe { PhysMapper::map_phys::<u64>(hpet_base_addr, 0x1000, PatTypes::Uncacheable) } {
                let hpet_registers = unsafe { core::slice::from_raw_parts(hpet_remap.get_vptr(), 0x1000 >> 3) };
                let hpet_gen = hpet_registers[0];
                let hpet_timer_count = ((hpet_gen & 0xf00) >> 8) + 1;
                info!("HPET @ {:#018x}->{:#018x}: Vendor: {:#06x}, Count: {}", hpet_base_addr, hpet_remap.get_vptr() as usize, 
                    (hpet_gen & 0x0ffff0000) >> 16, hpet_timer_count);
                    
                info!("HPET Period: {}fs", hpet_gen >> 32);
                info!("HPET Gen Reg: {:#018x}", hpet_gen);

                for n in 0..hpet_timer_count {
                    let timer_cfg = hpet_registers[((0x100 + (0x20 * n)) >> 3) as usize];
                    info!("HPET {} Reg: {:#018x}", n, timer_cfg);
                }

                Ok(Arc::new(Self {
                    mmio: AtomicPtr::<u64>::new(hpet_remap.get_vptr()),
                    num_timers: hpet_timer_count,
                }))
            } else {
                Err(DriverError::Initialization { reason: String::from("Failed mapping HPET") })?
            }
        } else {
            Err(DriverError::Initialization { reason: String::from("Invalid Data returned for ECAM") })
        }
    }

    fn set(&self, s: &str, val: Arc<dyn Any + Send + Sync>) -> core::result::Result<(), crate::driver::Error> {
        Err(DriverError::AssetNotProvided { name: s.into() })
    }

    fn get(&self, s: &str) -> core::result::Result<Arc<dyn Any + Sync + Send>, crate::driver::Error> {
        match s {
            _ => Err(crate::driver::Error::AssetNotProvided { name: String::from(s) }),
        }
    }
}

#[distributed_slice(DRIVERS)]
pub static HPET_DRIVER_RECORD: DriverEntry = DriverEntry {
    name: "hpet",
    req: &["hpet_info"],
    provides: &["timer"],
    ctor: HpetDriver::new
};
