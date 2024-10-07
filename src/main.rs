//#![feature(start, lang_items)]
#![no_std]
#![no_main]
#![feature(abi_x86_interrupt)]

mod allocator;
mod driver;
mod exceptions;
mod framebuffer;
mod interrupts;
mod kernel;
mod kernel_args;
mod klog;
mod memdrv;
mod paging;

use core::alloc::GlobalAlloc;
use core::arch::{asm, global_asm};
use core::panic::PanicInfo;
use core::ptr::addr_of_mut;
use kernel::Kernel;
use kernel_args::KernelArgs;
use log::{error, info, trace};
use snafu::prelude::*;
use uefi::mem::memory_map::MemoryType;

#[macro_use]
extern crate alloc;
use alloc::collections::BTreeMap;
use alloc::vec::Vec;

use allocator::{PageAllocator, KERN_ALLOC};
use klog::KernLogger;
use memdrv::{MemDriver, MEM_DRIVER};
use driver::DriverEntry;
use linkme::distributed_slice;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    error!("Panic, halting: {}", info);
    unsafe { asm!("hlt", options(noreturn)) };
}

#[derive(Debug, Snafu)]
enum Error {
    /// Error in Allocator: {source}
    Allocator { source: allocator::Error },
}

impl From<allocator::Error> for Error {
    fn from(ae: allocator::Error) -> Self {
        Self::Allocator { source: ae }
    }
}

type Result<T> = core::result::Result<T, Error>;

/*
 * A very simple "trampoline" that jumps us into Rust code.
 */
global_asm! {
    ".global _start",
    "_start:",
    "cli",
    "jmp kernmain",
}

pub static mut GKARG: *mut KernelArgs = core::ptr::null_mut();
static KLOG: KernLogger = KernLogger;

#[no_mangle]
fn kernmain(karg: &mut KernelArgs) -> ! {
    let karg2: KernelArgs = karg.clone();
    unsafe { GKARG = karg };
    log::set_logger(&KLOG).unwrap();
    log::set_max_level(log::LevelFilter::Debug);

    for a in 0..10 {
        info!("{:03} Hello from kernel", a);
    }
    info!("{:?}", karg2);

    let mm = karg2.get_memmap_slice();

    let mut pages: usize = 0;
    for a in mm.iter().filter(|x| x.ty == MemoryType::CONVENTIONAL) {
        //info!("{:?}\n", a);
        pages += a.pages as usize;
    }
    info!("Total mem: {}mb, {}pg", pages * 4096 / 1048576, pages);

    unsafe {
        MEM_DRIVER.init(karg);
    }

    // Initialize new 4MB kernel stack and switch to it
    let kernstack = if let Ok(x) = unsafe { MEM_DRIVER.palloc(1024) } {
        info!("Stack allocated: {:#018x}", x as usize);
        x
    } else {
        panic!("Failure to allocate stack");
    };

    // Execute the next phase of kernel start-up on the new stack we just
    // allocated
    unsafe {
        psm::replace_stack(kernstack, 4096 * 1024, || {
            let mut k = Kernel::new();
            k.start();
        })
    };
}
