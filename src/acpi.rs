use acpi::{
    AcpiError, AcpiTables,
    handler::{AcpiHandler, PhysicalMapping},
    hpet::HpetInfo,
    madt::IoApicEntry,
    mcfg::{PciConfigEntry, PciConfigRegions},
    platform::{
        interrupt::{
            IoApic,
            InterruptModel,
            InterruptSourceOverride,
            LocalInterruptLine,
            NmiLine,
            NmiProcessor,
            NmiSource,
        },
        PlatformInfo,
    },
};
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::any::Any;
use crate::GKARG;
use linkme::distributed_slice;
use log::{info, trace};
use snafu::prelude::*;
use crate::driver::{DRIVERS, Driver, DriverBus, DriverEntry, DriverError};
use crate::memdrv::PatTypes;
use crate::physmap::PhysMapper;

#[repr(C)]
#[derive(Copy, Clone, Debug)]
enum LApicLine {
    LINT0,
    LINT1,
}

impl From<LocalInterruptLine> for LApicLine {
    fn from(nmi_line: LocalInterruptLine) -> Self {
        match nmi_line {
            LocalInterruptLine::Lint0 => Self::LINT0,
            LocalInterruptLine::Lint1 => Self::LINT1,
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
enum NmiProc {
    All,
    Uid(u32),
}

impl From<NmiProcessor> for NmiProc {
    fn from(nmi_processor: NmiProcessor) -> Self {
        match nmi_processor {
            NmiProcessor::All => NmiProc::All,
            NmiProcessor::ProcessorUid(i) => NmiProc::Uid(i)
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct NmiInfo {
    cpu: NmiProc,
    line: LApicLine,
}

impl From<&NmiLine> for NmiInfo {
    fn from(nmi: &NmiLine) -> Self {
        Self {
            cpu: nmi.processor.into(),
            line: nmi.line.into(),
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug)]
pub struct LocalApicData {
    lapic_phys: usize,
    nmi_lines: Vec<NmiInfo>,
}

impl LocalApicData {
    pub fn get_phys_addr(&self) -> usize {
        self.lapic_phys
    }

    pub fn get_nmi_lines(&self) -> &Vec<NmiInfo> {
        &self.nmi_lines
    }
}

pub struct AcpiDriver {
    handler: VirtAcpiHandler,
    acpi: AcpiTables<VirtAcpiHandler>,
    hpet: Arc<HpetInfo>,
    lapic_phys: usize,
    ioapics: Vec<IoApic>,
    lapic_nmi_lines: Vec<NmiLine>,
    int_source_overrides: Vec<InterruptSourceOverride>,
    nmi_sources: Vec<NmiSource>,
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
    fn new(_bus: &mut dyn DriverBus) -> core::result::Result<Arc<dyn Driver>, crate::driver::Error> {
        Ok(Arc::new(Self::init()?))
    }

    fn set(&self, s: &str, val: Arc<dyn Any + Send + Sync>) -> core::result::Result<(), crate::driver::Error> {
        Err(DriverError::AssetNotProvided { name: s.into() })
    }

    fn get(&self, s: &str) -> core::result::Result<Arc<dyn Any + Sync + Send>, crate::driver::Error> {
        match s {
            "pcie_ecam" => {
                let pe = self.get_pcie_regions()?;
                info!("Ecams in match: {:?}", pe);
                Ok(Arc::new(AcpiDriverData::EcamPointer(pe)))
            },
            "hpet_info" => {
                Ok(self.hpet.clone())
            },
            "lapic" => {
                Ok(Arc::new(self.get_local_apic()?))
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
        let acpi_platform_info = PlatformInfo::new(&acpi)?;
        info!("Platform Info: {:?}", acpi_platform_info);

        let mut ioapics = Vec::new();
        let mut int_source_overrides = Vec::new();
        let mut lapic_nmi_lines = Vec::new();
        let mut nmi_sources = Vec::new();
        let mut lapic_phys = 0usize;
        if let InterruptModel::Apic(apic) = acpi_platform_info.interrupt_model {
            lapic_phys = apic.local_apic_address as usize;
            for ioapic in apic.io_apics.iter() {
                ioapics.push(IoApic { ..*ioapic });
            }
            for iso in apic.interrupt_source_overrides.iter() {
                int_source_overrides.push(InterruptSourceOverride { ..*iso });
            }
            for nmi_line in apic.local_apic_nmi_lines.iter() {
                lapic_nmi_lines.push(NmiLine { ..*nmi_line });
            }
            for nmi_src in apic.nmi_sources.iter() {
                nmi_sources.push(NmiSource { ..*nmi_src });
            }
        }
        info!("LAPIC paddr: {:#018x}", lapic_phys);
        for ioapic in ioapics.iter() {
            info!("IOAPIC #{} paddr: {:#018x}, start: {}", ioapic.id, ioapic.address as usize, ioapic.global_system_interrupt_base);
        }

        Ok(Self {
            handler,
            acpi,
            hpet: Arc::new(hpet_info),
            ioapics,
            int_source_overrides,
            lapic_nmi_lines,
            lapic_phys,
            nmi_sources,
        })
    }

    fn get_pcie_regions(&self) -> Result<Vec<EcamSegment>> {
        let pci_regions = PciConfigRegions::new(&self.acpi)?;
        let mut pcie_ecam = vec![];
        for pcir in pci_regions.iter() {
            info!("Pci Region: {:#018x} SG:{:#06x} Rng: {:#04x}-{:#04x}", pcir.physical_address, pcir.segment_group,
                pcir.bus_range.start(), pcir.bus_range.end());
            pcie_ecam.push(EcamSegment::from(pcir).clone());
        };
        info!("Ecams: {:?} {:#018x}", pcie_ecam, pcie_ecam.as_ptr() as usize);
        Ok(pcie_ecam)
    }

    fn get_local_apic(&self) -> Result<LocalApicData> {
        Ok(LocalApicData {
            lapic_phys: self.lapic_phys,
            nmi_lines: self.lapic_nmi_lines.iter().map(|x| x.into()).collect(),
        })
    }
}

#[distributed_slice(DRIVERS)]
pub static ACPI_DRIVER_RECORD: DriverEntry = DriverEntry {
    name: "acpi",
    req: &[],
    provides: &["pcie_ecam", "hpet_info", "acpi", "lapic"],
    ctor: AcpiDriver::new
};


impl AcpiHandler for VirtAcpiHandler {
    unsafe fn map_physical_region<T>(&self, physical_address: usize, size: usize) -> PhysicalMapping<Self, T> {
        if let Ok(vmap) = PhysMapper::map_phys(physical_address, size, PatTypes::Uncacheable) {
            let pm = PhysicalMapping::new(physical_address,
                core::ptr::NonNull::<T>::new_unchecked(vmap.get_vptr()),
                size,
                vmap.get_size(),
                self.clone());
            trace!("Mapped {:#018x} len {:#018x} to {:#018x}", pm.physical_start(), pm.mapped_length(), vmap.get_vptr() as usize);
            return pm;
        }

        panic!("Couldn't find frames to map for ACPI!")
    }

    fn unmap_physical_region<T>(region: &PhysicalMapping<Self, T>) {
        trace!("Unmap for {:#018x} len {:#018x} at {:#018x}", region.physical_start(), region.mapped_length(),
              region.virtual_start().as_ptr() as usize);

        PhysMapper::unmap(region.virtual_start().as_ptr(), region.mapped_length());

    }
}
