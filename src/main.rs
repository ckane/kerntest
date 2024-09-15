//#![feature(start, lang_items)]
#![no_std]
#![no_main]

mod allocator;
mod framebuffer;
mod kernel_args;
mod klog;
mod memdrv;
mod paging;

use core::alloc::GlobalAlloc;
use core::arch::{asm, global_asm};
use core::fmt::Write;
use core::panic::PanicInfo;
use core::ptr::addr_of_mut;
use kernel_args::KernelArgs;
use log::{error, info, trace};
use snafu::prelude::*;
use uefi::mem::memory_map::MemoryType;

extern crate alloc;
use alloc::collections::BTreeMap;
use alloc::vec::Vec;

use allocator::{PageAllocator, KERN_ALLOC};
use klog::KernLogger;
use memdrv::{MemDriver, MEM_DRIVER};

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    unsafe { asm!("hlt", options(noreturn)) };
}

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

    match MemDriver::vtop(0xffffe00000019001) {
        Ok(x) => info!("phys: {:#018x}", x),
        Err(e) => error!("Failed calculating physaddr: {}", e)
    }

    match MemDriver::vtop(0xffffe0000001a001) {
        Ok(x) => info!("phys: {:#018x}", x),
        Err(e) => error!("Failed calculating physaddr: {}", e)
    }

    match MemDriver::vtop(0xffffe0000001b001) {
        Ok(x) => info!("phys: {:#018x}", x),
        Err(e) => error!("Failed calculating physaddr: {}", e)
    }

    match MemDriver::vtop(0xffffd00000000001) {
        Ok(x) => info!("phys: {:#018x}", x),
        Err(e) => error!("Failed calculating physaddr: {}", e)
    }

    let numpgs = 300000;
    match unsafe { MEM_DRIVER.palloc(numpgs) } {
        Ok(x) => {
            info!("Allocated {} pages @ {:#018x}", numpgs, x as usize);
            let _ = unsafe { MEM_DRIVER.pfree(x, numpgs) };
            info!("Freed {} pages @ {:#018x}", numpgs, x as usize);
        }
        Err(e) => {
            error!("{}", e);
        }
    }

    // Set up the KernelAlloc
    unsafe { KERN_ALLOC.set_page_allocator(addr_of_mut!(MEM_DRIVER)) };

    info!("Initialized Kernel Allocator");

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
        unsafe { KERN_ALLOC.dealloc(*i, core::alloc::Layout::from_size_align_unchecked(sz, 8)) };
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

    unsafe { asm!("hlt", options(noreturn)) };
}
