use acpi::{AcpiError, AcpiTables, handler::{AcpiHandler, PhysicalMapping}, hpet::HpetInfo, mcfg::{PciConfigEntry, PciConfigRegions}, platform::PlatformInfo};
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::any::Any;
use crate::GKARG;
use linkme::distributed_slice;
use log::{error, info, trace};
use snafu::prelude::*;
use crate::driver::{DRIVERS, Driver, DriverEntry};
use crate::memdrv::{MEM_DRIVER, MemDriver};

pub struct AcpiDriver {
    handler: VirtAcpiHandler,
    acpi: AcpiTables<VirtAcpiHandler>,
    hpet: HpetInfo,
}

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct EcamSegment {
    paddr: usize,
    bus_start: u8,
    bus_end: u8,
    group: u16,
}

impl EcamSegment {
    pub fn phys_addr(&self) -> usize {
        self.paddr
    }

    pub fn bus_start(&self) -> u8 {
        self.bus_start
    }

    pub fn bus_end(&self) -> u8 {
        self.bus_end
    }

    pub fn segment_group(&self) -> u16 {
        self.group
    }
}

#[derive(Clone, Debug)]
pub enum AcpiDriverData {
    EcamPointer(Vec<EcamSegment>),
}

#[derive(Clone, Copy, Debug)]
struct VirtAcpiHandler;

#[derive(Snafu, Debug)]
enum Error {
    /// ACPI Subsystem error
    Acpi { acpi_detail: acpi::AcpiError }
}

type Result<T> = core::result::Result<T, Error>;

struct IntData {
    avail: Vec<usize>,
}

impl IntData {
    /// Create a new VirtAcpiHandler with an available allocation list of
    /// {size} 256MB mappings
    fn initialize(&mut self, size: usize) {
        for i in 0..size {
            self.avail.push(i)
        }
    }
}

impl From<AcpiError> for Error {
    fn from(acpierr: AcpiError) -> Self {
        Self::Acpi { acpi_detail: acpierr }
    }
}

impl From<Error> for crate::driver::Error {
    fn from(err: Error) -> Self {
        Self::Initialization { reason: format!("{:?}", err) }
    }
}

impl From<PciConfigEntry> for EcamSegment {
    fn from(e: PciConfigEntry) -> Self {
        let f = Self {
            paddr: e.physical_address,
            group: e.segment_group,
            bus_start: e.bus_range.start().clone(),
            bus_end: e.bus_range.end().clone(),
        };
        info!("Constructed EcamSegment: {:?}", f);
        f
    }
}

impl Driver for AcpiDriver {
    fn new() -> core::result::Result<Arc<dyn Driver>, crate::driver::Error> {
        Ok(Arc::new(Self::init()?))
    }

    fn get(&self, s: &str) -> core::result::Result<Arc<dyn Any>, crate::driver::Error> {
        match s {
            "pcie_ecam" => {
                let mut pe = self.get_pcie_regions()?;
                info!("Ecams in match: {:?}", pe);
                Ok(Arc::new(AcpiDriverData::EcamPointer(pe)))
            },
            _ => Err(crate::driver::Error::AssetNotProvided { name: String::from(s) }),
        }
    }
}

impl AcpiDriver {
    fn init() -> Result<Self> {
        let (ap, _ver) = unsafe { (*GKARG).get_acpi() };
        let acpi_phys = ap as usize;
        let handler = VirtAcpiHandler;
        let acpi = unsafe { AcpiTables::from_rsdp(handler, acpi_phys) }?;
        info!("ACPI Structure loaded! {:?}", acpi);
        let hpet_info = HpetInfo::new(&acpi)?;
        info!("HPET Info: {:?}", hpet_info);
        Ok(Self { handler, acpi, hpet: hpet_info })
    }

    fn get_pcie_regions(&self) -> Result<Vec<EcamSegment>> {
        let pci_regions = PciConfigRegions::new(&self.acpi)?;
        let mut pcie_ecam = vec![];
        for pcir in pci_regions.iter() {
            info!("Pci Region: {:#018x} SG:{:#06x} Rng: {:#04x}-{:#04x}", pcir.physical_address, pcir.segment_group,
                pcir.bus_range.start(), pcir.bus_range.end());
            pcie_ecam.push(EcamSegment::from(pcir));
        };
        info!("Ecams: {:?}", pcie_ecam);
        Ok(pcie_ecam)
    }
}

#[distributed_slice(DRIVERS)]
pub static ACPI_DRIVER_RECORD: DriverEntry = DriverEntry {
    name: "acpi",
    req: &[],
    provides: &["pcie_ecam", "hpet_info", "acpi"],
    ctor: AcpiDriver::new
};


static mut INT_DATA: IntData = IntData {
    avail: vec![],
};

/// Each mapped segment is 256MB
const SEG_SHIFT: usize = 12;
const MAP_SIZE: usize = 1usize << SEG_SHIFT;
const MAP_BASE: usize = 0xffffc00000000000;
const MAP_COUNTS: usize = 1024;

impl AcpiHandler for VirtAcpiHandler {
    unsafe fn map_physical_region<T>(&self, physical_address: usize, size: usize) -> PhysicalMapping<Self, T> {
        // If INT_DATA uninitialized, then initialize it
        if INT_DATA.avail.is_empty() {
            INT_DATA.initialize(MAP_COUNTS)
        }

        trace!("Req: {:#018x} len {:#018x} T: {}", physical_address, size, core::mem::size_of::<T>());

        // Calculate the physical base addr on its nearest page boundary
        let pbase = physical_address & !0xfffusize;

        // Determine what the size needs to be, adjusted as 4kB pages
        let adj_size = (size + (MAP_SIZE - 1)) & !(MAP_SIZE - 1);

        // First find out how many segments will be needed
        let num_seg = (adj_size + (MAP_SIZE - 1)) / MAP_SIZE;

        // Then, determine where in self.avail we have a sequence that's that long
        let mut sequence = false;
        let mut seq_idx = 0;
        for (j, w) in INT_DATA.avail.windows(num_seg).enumerate() {
            sequence = true;
            for (i, val) in w.iter().enumerate() {
                if *val != w[0] + i {
                    sequence = false;
                    break;
                } 
            }
            if sequence {
                seq_idx = j;
                break;
            }
        };

        // If we validated the set was a sequence, then perform a mapping across it
        if sequence {
            let vseg_start = INT_DATA.avail[seq_idx];
            for _ in 0..num_seg {
                let vseg = INT_DATA.avail.remove(seq_idx);
                for p in 0..(MAP_SIZE >> 12) {
                    MEM_DRIVER.map_vtop(MAP_BASE + (vseg << SEG_SHIFT) + (p << 12usize), pbase + (p << 12usize));
                }
            }

            let struct_addr = MAP_BASE + (vseg_start << SEG_SHIFT) + (physical_address & 0xfffusize);
            let pm = PhysicalMapping::new(physical_address,
                                          core::ptr::NonNull::<T>::new_unchecked(struct_addr as *mut T),
                                          size,
                                          adj_size,
                                          self.clone());
            trace!("Mapped {:#018x} len {:#018x} to {:#018x}", pm.physical_start(), pm.mapped_length(), struct_addr);
            return pm;

        }
        panic!("Couldn't find frames to map for ACPI!")
    }

    fn unmap_physical_region<T>(region: &PhysicalMapping<Self, T>) {
        trace!("Unmap for {:#018x} len {:#018x} at {:#018x}", region.physical_start(), region.mapped_length(),
              region.virtual_start().as_ptr() as usize);

        let vstart = ((region.virtual_start().as_ptr() as usize) - 0xffffc00000000000usize) >> SEG_SHIFT;

        // Scan the available segments list for the sorted position where the requested freed
        // segment belongs
        for i in 0..unsafe { INT_DATA.avail.len() } {
            let ival = unsafe { INT_DATA.avail[i] };
            if ival > vstart {
                trace!("Pos: {}/{:#018x} > {:#018x}", i, ival, vstart);
                let end_segs = vstart + (region.mapped_length() + (MAP_SIZE - 1)) / MAP_SIZE;

                for s in vstart..end_segs {
                    // Unmap all the backing pages
                    for p in 0..(MAP_SIZE >> 12) {
                        trace!("Unmapping: {:#018x}", MAP_BASE + ((vstart + end_segs - s - 1usize) << SEG_SHIFT) + (p << 12usize));
                        unsafe { MemDriver::unmap_vmap(MAP_BASE + ((vstart + end_segs - s - 1usize) << SEG_SHIFT) + (p << 12usize)) };
                    }

                    // Insert the freed segment back on the segment list in the appropriate sorted
                    // position
                    unsafe { INT_DATA.avail.insert(i, vstart + end_segs - s - 1) };
                    unsafe { trace!("New avail head: {:#018x}", INT_DATA.avail.first().unwrap()) };
                }
                break;
            }
        }
    }
}
