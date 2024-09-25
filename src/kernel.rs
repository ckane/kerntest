use crate::allocator::{PageAllocator, KERN_ALLOC};
use crate::interrupts::{
    Gdtr, GlobalDescriptorEntry, Idtr, InterruptDescriptorEntry, InterruptStackTable,
};
use crate::memdrv::{MemDriver, MEM_DRIVER};
use alloc::collections::BTreeMap;
use alloc::vec::Vec;
use core::alloc::GlobalAlloc;
use core::arch::asm;
use core::ptr::addr_of_mut;
use log::{error, info, trace};
use snafu::prelude::*;

pub struct Kernel {
    gdtr: Gdtr,
    gdt: Vec<GlobalDescriptorEntry>,
    idtr: Idtr,
    idt: Vec<InterruptDescriptorEntry>,
    ist: InterruptStackTable,
}

fn int3_panic() -> ! {
    panic!("INT3 fired!")
}

fn df_panic() -> ! {
    panic!("Double fault fired!")
}

#[derive(Debug, Snafu)]
pub(crate) enum Error {
    /// Error in Allocator: {source}
    Allocator { source: crate::allocator::Error },
}

impl From<crate::allocator::Error> for Error {
    fn from(ae: crate::allocator::Error) -> Self {
        Self::Allocator { source: ae }
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
        idt[3] = InterruptDescriptorEntry::new_trap(int3_panic as usize, 0x10, 0, 1);
        idt[8] = InterruptDescriptorEntry::new_trap(df_panic as usize, 0x10, 0, 0);

        Self {
            // Will delay the construction of the GDT until later on when
            // ready to LGDT
            gdtr: Gdtr::new(&[]),
            gdt: Vec::new(),
            idtr: Idtr::new(idt.as_slice()),
            idt: idt,
            ist: InterruptStackTable::default(),
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
        self.idtr.lidt();
        info!("Initialized IDT");
        for i in 0..self.idt.len() {
            let ent = u128::from(&self.idt[i]);
            if ent != 0 {
                info!("IDT entry: {:#034x}", ent);
            }
        }
        info!("Idt addr: {:#018x}", self.idt.as_mut_ptr() as usize);
    }

    pub fn start(&mut self) {
        // The last address in this list should throw an Err()
        info!("Starting kernel instance");
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
                    panic!("Failure unexpected testing page {page:#018x}, {pt:#018x}");
                }
            }
        }
        info!("vtop tests passed (above error is expected)");

        match Self::test_palloc(300000) {
            Ok(_) => info!("page alloc tests passed"),
            Err(e) => {
                panic!("Page alloc tests failed: {e}");
            }
        }

        unsafe {
            info!("KERN_ALLOC: {:?}", KERN_ALLOC);
        }

        let f = &mut [core::ptr::null_mut(); 6097];

        for i in f.iter_mut() {
            let sz = 43;
            *i = unsafe { KERN_ALLOC.alloc(core::alloc::Layout::from_size_align_unchecked(sz, 8)) };
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

        self.initialize_gdt();
        self.initialize_ist();
        self.initialize_idt();

        unsafe {asm!(
            "int3",
            options(nomem, nostack)
        )};

        panic!("End of kernel execution");
    }
}
