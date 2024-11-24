use crate::memdrv::{MemDriver, MEM_DRIVER};

use alloc::vec::Vec;
use log::{info, trace};
use snafu::prelude::*;

#[derive(Clone, Debug, Snafu)]
pub(crate) enum Error {
    /// Allocation failure due to no free mappings
    NoMoreMappings,
}

type Result<T> = core::result::Result<T, Error>;

pub(crate) struct PhysMapper;

struct IntData {
    avail: Vec<usize>,
}

pub(crate) struct PhysMap<T> {
    vptr: *mut T,
    size: usize,
}

impl<T> PhysMap<T> {
    pub fn get_vptr(&self) -> *mut T {
        self.vptr
    }
    pub fn get_size(&self) -> usize {
        self.size
    }
}

impl IntData {
    /// Create a new VirtAcpiHandler with an available allocation list of
    /// {size} {MAP_SIZE} mappings
    fn initialize(&mut self, size: usize) {
        for i in 0..size {
            self.avail.push(i)
        }
    }
}

static mut INT_DATA: IntData = IntData {
    avail: vec![],
};

/// Each mapped segment is 4kB
const SEG_SHIFT: usize = 13;
const MAP_SIZE: usize = 1usize << SEG_SHIFT;
const MAP_BASE: usize = 0xffffc00000000000;
const MAP_COUNTS: usize = 10240;

impl PhysMapper {
    pub unsafe fn map_phys<T>(phys_addr: usize, size: usize, pat: crate::memdrv::PatTypes) -> Result<PhysMap<T>> {
        // If INT_DATA uninitialized, then initialize it
        if INT_DATA.avail.is_empty() {
            INT_DATA.initialize(MAP_COUNTS)
        }

        trace!("Req: {:#018x} len {:#018x} T: {}", phys_addr, size, core::mem::size_of::<T>());

        // Calculate the physical base addr on its nearest page boundary
        let pbase = phys_addr & !0xfffusize;

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
            for pseg in 0..num_seg {
                let vseg = INT_DATA.avail.remove(seq_idx);
                for p in 0..(MAP_SIZE >> 12) {
                    MEM_DRIVER.map_vtop(
                        MAP_BASE + (vseg << SEG_SHIFT) + (p << 12usize),
                        pbase + (pseg << SEG_SHIFT) + (p << 12usize),
                        pat
                    );
                }
            }

            let struct_addr = MAP_BASE + (vseg_start << SEG_SHIFT) + (phys_addr & 0xfffusize);
            Ok(PhysMap::<T> { vptr: struct_addr as *mut T, size: adj_size })
        } else {
            Err(Error::NoMoreMappings)
        }
    }

    pub fn unmap<T>(vaddr: *mut T, size: usize) {
        let vstart = ((vaddr as usize) - 0xffffc00000000000usize) >> SEG_SHIFT;

        // Scan the available segments list for the sorted position where the requested freed
        // segment belongs
        for i in 0..unsafe { INT_DATA.avail.len() } {
            let ival = unsafe { INT_DATA.avail[i] };
            if ival > vstart {
                trace!("Pos: {}/{:#018x} > {:#018x}", i, ival, vstart);
                let end_segs = vstart + (size + (MAP_SIZE - 1)) / MAP_SIZE;

                for s in vstart..end_segs {
                    // Unmap all the backing pages
                    for p in 0..(MAP_SIZE >> 12) {
                        trace!("Unmapping: {:#018x}", MAP_BASE + ((vstart + end_segs - s - 1usize) << SEG_SHIFT) + (p << 12usize));
                        MemDriver::unmap_vmap(MAP_BASE + ((vstart + end_segs - s - 1usize) << SEG_SHIFT) + (p << 12usize));
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
