use crate::{GKARG, KERNEL, KLOG};
use crate::acpi::AcpiDriver;
use crate::cpu::{Processor, add_cpu, add_thread};
use crate::allocator::{PageAllocator, KERN_ALLOC};
use crate::driver::{DRIVERS, Driver, DriverBus, DriverEntry};
use crate::exceptions::{attach_exceptions, timer_queued};
use crate::interrupts::{
    Gdtr, GlobalDescriptorEntry, Idtr, InterruptDescriptorEntry, InterruptStack,
    InterruptStackTable,
};
use crate::klog::KernLogger;
use crate::memdrv::{MemDriver, MEM_DRIVER};
use crate::thread::Thread;
use alloc::collections::{BTreeMap, BTreeSet};
use alloc::string::String;
use alloc::vec::Vec;
use core::alloc::GlobalAlloc;
use core::any::Any;
use core::arch::asm;
use core::fmt::Write;
use core::ops::Deref;
use core::ptr::addr_of_mut;
use log::{error, info, trace};
use snafu::prelude::*;
use alloc::boxed::Box;
use alloc::sync::Arc;
use core::sync::atomic::{AtomicPtr, Ordering};

pub struct Kernel {
    gdtr: Gdtr,
    gdt: Vec<GlobalDescriptorEntry>,
    idtr: Idtr,
    idt: Vec<InterruptDescriptorEntry>,
    ist: InterruptStackTable,
    drivers: BTreeMap<String, Arc<dyn Driver>>,
    cpus: Vec<Processor>,
}

#[derive(Debug, Snafu)]
pub(crate) enum Error {
    /// Error in Allocator: {source}
    Allocator { source: crate::allocator::Error },

    /// Errors produced while testing Allocator
    AllocatorTest { description: String },

    /// ACPI Subsystem error
    Acpi { acpi_detail: acpi::AcpiError },

    /// Driver subsystem error
    Driver { source: crate::driver::Error }
}

impl From<acpi::AcpiError> for Error {
    fn from(acpierr: acpi::AcpiError) -> Self {
        Self::Acpi { acpi_detail: acpierr }
    }
}

impl From<crate::allocator::Error> for Error {
    fn from(ae: crate::allocator::Error) -> Self {
        Self::Allocator { source: ae }
    }
}

impl From<crate::driver::Error> for Error {
    fn from(drverr: crate::driver::Error) -> Self {
        Self::Driver { source: drverr }
    }
}

pub type Result<T> = core::result::Result<T, Error>;

impl Kernel {
    pub fn new() -> Self {
        // Set up the KernelAlloc
        unsafe { KERN_ALLOC.set_page_allocator(addr_of_mut!(MEM_DRIVER)) };

        info!("Initialized Kernel Allocator");

        info!("Allocating new kernel");

        // Initialize the IDT and register some handlers
        let mut idt = vec![InterruptDescriptorEntry::default(); 256];
        attach_exceptions(&mut idt);

        Self {
            // Will delay the construction of the GDT until later on when
            // ready to LGDT
            gdtr: Gdtr::new(&[]),
            gdt: Vec::new(),
            idtr: Idtr::new(idt.as_slice()),
            idt: idt,
            ist: InterruptStackTable::default(),
            drivers: BTreeMap::new(),
            cpus: vec![],
        }
    }

    fn test_vtop(addrs: &[usize]) -> Result<()> {
        for a in addrs {
            let phys = MemDriver::vtop(*a).map_err(|x| Error::Allocator { source: x })?;
            info!("phys: {:#018x}", phys);
        }
        Ok(())
    }

    fn test_palloc(numpgs: usize) -> Result<()> {
        let x: *mut u8 = unsafe { MEM_DRIVER.palloc(numpgs) }?;
        info!("Allocated {} pages @ {:#018x}", numpgs, x as usize);

        // Ignoring any failures from free
        let _ = unsafe { MEM_DRIVER.pfree(x, numpgs) }?;

        info!("Freed {} pages @ {:#018x}", numpgs, x as usize);
        Ok(())
    }

    fn initialize_gdt(&mut self) {
        self.gdt.push(GlobalDescriptorEntry::default());
        self.gdt.push(GlobalDescriptorEntry::new_codeseg(0));
        self.gdt.push(GlobalDescriptorEntry::new_dataseg(0));

        if let (Some(istp), istlen) = (self.ist.get_ist_ptr(), self.ist.get_ist_len_bytes()) {
            self.gdt.push(GlobalDescriptorEntry::new_istseg(istp as usize, istlen));
            info!("IST pointer: {:#018x}", istp as usize);
        } else {
            panic!("IST failed to initialize or is corrupted");
        }
        self.gdtr = Gdtr::new(&self.gdt);

        info!("gdtr: {:#022x}", u128::from(&self.gdtr));
        info!("idt : {:#018x}", self.idt.as_ptr() as usize);
        info!("idtr: {:#022x}", u128::from(&self.idtr));

        for (ii, gg) in self.gdt.iter().enumerate() {
            info!("GDT{:#04x}: {:#034x}", ii, u128::from(gg));
        }

        self.gdtr.lgdt(0x10, 0x20);
        info!("Loaded new GDTR and transferred execution");
        unsafe { asm!(
            "mov rax, {}",
            "ltr ax",
            in(reg) 0x30 as usize,
        )};
        info!("Loaded new IST from GDTR");
    }

    fn initialize_ist(&mut self) {
        info!("IST: {:?}", self.ist);
        info!("Initialized new IST");
    }

    fn initialize_idt(&mut self) {
        self.idtr.disable_legacy_pic();
        info!("Disabled Legacy PIC");
        self.idtr.lidt();
        info!("Initialized IDT");
        for i in 0..self.idt.len() {
            let ent = u128::from(&self.idt[i]);
            if ent != 0 {
                trace!("IDT entry: {:#034x}", ent);
            }
        }
        info!("Idt addr: {:#018x}", self.idt.as_mut_ptr() as usize);
    }

    fn test_allocator(&mut self) -> Result<()> {
        let testlist = &[
            0xffffe00000019001,
            0xffffe0000001a001,
            0xffffe0000001b001,
            0xffffd00000000001,
        ];
        if let Err(top_err) = Self::test_vtop(testlist) {
            error!("Failed calculating physaddr: {}", top_err);

            // Validate that the only failure was the last entry reporting as unmapped
            if let Error::Allocator {
                source: crate::allocator::Error::UnmappedPage { page, pt },
            } = top_err
            {
                if page != testlist[testlist.len() - 1] {
                    Err(Error::AllocatorTest {
                        description: format!("Failure unexpected testing page {page:#018x}, {pt:#018x}")
                    })?
                }
            }
        }
        info!("vtop tests passed (above error is expected)");

        Self::test_palloc(300000).map_err(|e| {
            Error::AllocatorTest {
                description: format!("Page alloc tests failed: {e}")
            }
        })?;

        let f = &mut [core::ptr::null_mut(); 6097];

        for i in f.iter_mut() {
            let sz = 43;
            *i = unsafe { KERN_ALLOC.alloc(core::alloc::Layout::from_size_align_unchecked(sz, 8)) };
            if (*i).is_null()  {
                Err(Error::AllocatorTest { 
                    description: format!("Failed to alloc in 6097 loop")
                })?
            }
            trace!("Allocated {} bytes: {:#018x}", sz, *i as usize);
        }

        info!("Done w/ Allocation Test");

        for i in f.iter_mut() {
            let sz = 43;
            trace!("Deallocating: {:#018x}", *i as usize);
            unsafe {
                KERN_ALLOC.dealloc(*i, core::alloc::Layout::from_size_align_unchecked(sz, 8))
            };
        }

        info!("Finished deallocation");

        let mut v = Vec::new();
        v.push(999999999usize);
        v.push(0usize);
        v.push(104usize);

        info!("v: {:?}", &v);

        let mut bt = BTreeMap::new();
        bt.insert("helllo", "foobar");
        bt.insert("welcome", "you are");
        bt.insert("ninety", "nine");

        info!("bt: {:?}", &bt);

        Ok(())
    }

    fn register_driver(&mut self, d: &DriverEntry) {
        if let Ok(drv) = (d.ctor)(self) {
            self.drivers.insert(String::from(d.name), drv);
            info!("Attached drv <{}> which provides {:?}", d.name, d.provides);
        }
    }

    fn map_drivers(&mut self) -> Result<()> {
        let mut d_provs = BTreeSet::<&str>::new();
        let mut completed = 0;

        // First load all drivers with no pre-reqs
        for d in DRIVERS.iter().filter(|x| x.req.len() == 0) {
            self.register_driver(d);
            for p in d.provides {
                d_provs.insert(p);
                if String::from(*p) == String::from("log") {
                    info!("Registering new logger: {}", d.name);
                }
            };
            completed += 1;
        };

        while completed < DRIVERS.len() {
            // Then, load all drivers with pre-reqs
            let d_provs_c = d_provs.clone();
            for d in DRIVERS.iter().filter(|x| x.req.len() > 0 && x.req.iter().all(|y| d_provs_c.contains(y))) {
                self.register_driver(d);
                for p in d.provides {
                    d_provs.insert(p);
                };
                completed += 1;
            };
        };

        Ok(())
    }

    pub fn start(&mut self) {
        // The last address in this list should throw an Err()
        info!("Starting kernel instance");

        // Perform allocator validation
        if let Err(Error::AllocatorTest { description }) = self.test_allocator() {
            panic!("Allocator testing failed: {}", description)
        }

        self.initialize_gdt();
        self.initialize_ist();
        self.initialize_idt();

        unsafe {asm!(
            "int3",
            options(nomem, nostack)
        )};

        let core_id = 0u64;
        unsafe { asm!(
            "mov rdx, {}",
            "mov rax, rdx",
            "shr rdx, 32",
            "mov ecx, 0xc0000103",
            "wrmsr",
            in(reg) core_id
        ) };
        self.cpus.push(Processor::new(0));
        add_cpu();

        let th1 = Thread::new(1, 0x20, 0x10, thread1 as u64);
        let th2 = Thread::new(2, 0x20, 0x10, thread2 as u64);
        add_thread(0, th1);
        add_thread(0, th2);

        if let Err(x) = self.map_drivers() {
            panic!("ACPI Error: {:?}", x);
        }

        // Enable interrupts after initializing built-in drivers
        crate::cpu::start_ints();

        // Have exceptions.rs set a "fired" flag in the timer int
        // Then have a check after the "hlt" here which will look for
        // the timer flag to be set, and if it is set, send an EOI
        // to the appropriate driver on the driver bus
        loop {
            // Handle any outstanding INTs
            while timer_queued() {
                // Send the timerint signal to the bus
                self.set("timerint", Arc::new(true)).unwrap();
            }
            info!("KERNEL HALTED");
            unsafe { asm!("hlt") };
        }
    }
}

extern "C" fn thread1() {
    info!("Thread 1");
    loop {
        /*while timer_queued() {
            unsafe { KERNEL[0].set("timerint", Arc::new(true)).unwrap() };
        }*/
        info!("Thread 1 Halting");
        unsafe { asm!("hlt") };
        info!("Thread 1 Awoke");
    }
}

extern "C" fn thread2() {
    info!("Thread 2");
    loop {
        /*while timer_queued() {
            unsafe { KERNEL[0].set("timerint", Arc::new(true)).unwrap() };
        }*/
        info!("Thread 2 Halting");
        unsafe { asm!("hlt") };
        info!("Thread 2 Awoke");
    }
}

impl DriverBus for Kernel {
    fn set(&self, s: &str, val: Arc<dyn Any + Sync +Send>) -> core::result::Result<(), crate::driver::Error> {
        // First find the appropriate provider of s
        for d in DRIVERS.iter() {
            if let Some(_) = d.provides.iter().position(|&x| x == s) {
                trace!("Found [{}] at driver [{}]", s, d.name);
                return self.drivers.get(d.name)
                    .and_then(|x| x.set(s, val).ok())
                    .ok_or(crate::driver::Error::AssetFatalError { name: String::from(s) });
            };
        };

        // If a scan of the list doesn't find the attribute we're searching for,
        // then report that it doesn't exist
        Err(crate::driver::Error::AssetNotProvided { name: String::from(s) })
    }

    fn get(&self, s: &str) -> core::result::Result<Arc<dyn Any + Sync + Send>, crate::driver::Error> {
        // First find the appropriate provider of s
        for d in DRIVERS.iter() {
            if let Some(_) = d.provides.iter().position(|&x| x == s) {
                trace!("Found [{}] at driver [{}]", s, d.name);
                return self.drivers.get(d.name)
                    .and_then(|x| x.get(s).ok())
                    .ok_or(crate::driver::Error::AssetFatalError { name: String::from(s) });
            };
        };

        // If a scan of the list doesn't find the attribute we're searching for,
        // then report that it doesn't exist
        Err(crate::driver::Error::AssetNotProvided { name: String::from(s) })
    }
}
