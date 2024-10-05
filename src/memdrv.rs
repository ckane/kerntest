use crate::allocator::PageAllocator;
use crate::kernel_args::{KernelArgs, OSMemEntry};
use crate::paging::PDEntry;
use core::arch::asm;
use core::slice::from_raw_parts_mut;
use log::{info, trace};
use snafu::prelude::*;
use uefi::mem::memory_map::MemoryType;

#[derive(Debug, Snafu)]
enum Error {
    /// Collision attempting to map to an already-mapped page.
    /// vp {vpage:#018x} already maps to pp {ppage:#018x}
    MapCollision { vpage: usize, ppage: usize },

    /// Insufficient free memory
    Allocator { source: crate::allocator::Error }
}

impl From<crate::allocator::Error> for Error {
    fn from(ae: crate::allocator::Error) -> Self {
        Self::Allocator { source: ae }
    }
}

type Result<T> = core::result::Result<T, Error>;

/*
 * Need to:
 * Allocate physical pages for new kernel-owned page table
 * - Needs to map kernel memory + RUNTIME_SERVICES allocations
 * - Needs to map the console (same VMEM as received)
 * - Needs to map new page stack (2 u64's per page)
 * - Need to allocate page stack (2 u64's per page)
 * - Need to populate page stack with all CONVENTIONAL pages
 * - Need to install new page table
 *
 * Future: While we're at it, we can walk the old page tables
 *         and reclaim the pages that were used to store them
 *         onto the end of the page stack
 */
#[derive(Copy, Clone, Debug)]
struct MemPage(pub usize);

impl From<MemPage> for usize {
    fn from(m: MemPage) -> Self {
        m.0
    }
}

impl From<usize> for MemPage {
    fn from(i: usize) -> Self {
        Self(i)
    }
}

#[derive(Debug)]
pub struct MemDriver {
    pml4: &'static mut [PDEntry],
    dynbase: MemPage,
    dynstart: MemPage,
    memmap: SysMemMap,

    /// Slice which acts as the stack of free pages
    free_pages: &'static mut [MemPage],

    /// Cursor index to the first available free page in the stack
    ifp: usize,
}

#[derive(Copy, Clone, Debug)]
pub struct SysMemMap {
    memmap: &'static [OSMemEntry],
}
struct SysMemMapPageIter<'a> {
    sysmm: &'a SysMemMap,
    mapentry: usize,
    ty: uefi::table::boot::MemoryType,
    cur: usize,
}

impl SysMemMap {
    fn new(mm: &'static [OSMemEntry]) -> Self {
        Self { memmap: mm }
    }

    fn iter<'a>(&'a self, ty: uefi::table::boot::MemoryType) -> SysMemMapPageIter<'a> {
        SysMemMapPageIter {
            sysmm: self,
            ty,
            cur: 0,
            mapentry: 0,
        }
    }
}

impl<'a> Iterator for SysMemMapPageIter<'a> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        if self.sysmm.memmap.len() <= self.mapentry {
            None
        } else if self.sysmm.memmap[self.mapentry].pages <= self.cur
            || self.sysmm.memmap[self.mapentry].ty != self.ty
        {
            self.mapentry += 1;
            self.cur = 0;
            self.next()
        } else {
            self.cur += 1;
            Some(self.sysmm.memmap[self.mapentry].base + (self.cur - 1) * 0x1000)
        }
    }
}

impl PageAllocator for MemDriver {
    /// Frees {count} pages and returns them to the page stack
    fn pfree(&mut self, ptr: *mut u8, count: usize) -> crate::allocator::Result<()> {
        let mut lptr = ptr as usize;
        for _ in 0..count {
            // Be sure to round to nearest page when freeing
            self.pmap_free(lptr & !0xfff).map(|_| ())?;
            Self::pinvalidate(lptr);
            lptr = lptr + 0x1000;
        }
        Ok(())
    }

    /// Allocates the requested number of pages and returns the
    /// vmem pointer to them
    fn palloc(&mut self, pages: usize) -> crate::allocator::Result<*mut u8> {
        if pages + self.ifp > self.free_pages.len() {
            Err(crate::allocator::Error::InsufficientFree)
        } else {
            /* Map phys to vmem and return pointer to first vmem page. */
            self.pmap(pages)
        }
    }
}

impl MemDriver {
    pub fn init(&mut self, karg: &mut KernelArgs) {
        self.pml4 = Self::get_pml4();

        // Gets initialized to the first vpage after the kernel
        self.dynbase = MemPage(karg.get_kernbase() as usize + karg.get_kernpages() * 0x1000);

        // Will be initialized by alloc_pagestack()
        self.free_pages = &mut [];
        self.ifp = 0;
        self.dynstart = 0.into();

        // Stores the memory map passed into the kernel by the UEFI bootloader
        self.memmap = SysMemMap::new(karg.get_memmap_slice());
        self.init_recursive();
        self.alloc_pagestack();
    }

    fn iter_physpage<'a>(&'a self) -> SysMemMapPageIter<'a> {
        self.memmap.iter(MemoryType::CONVENTIONAL)
    }

    fn alloc_pagestack(&mut self) {
        let conv_page_count = self.iter_physpage().count();
        let pages_needed = conv_page_count * core::mem::size_of::<MemPage>() / 0x1000;
        let mut vmem_cursor = self.dynbase.0 as usize;

        // Initialize the start of dynamic RAM past the page stack
        self.dynstart = MemPage(self.dynbase.0 + conv_page_count * core::mem::size_of::<MemPage>());

        // If the vmem_cursor isn't on a page boundary, set it to the next-highest page
        // boundary
        vmem_cursor = (vmem_cursor + 0xfff) & !0xfff;

        // This iterator starts yielding available physical pages that begin
        // after the physical pages needed for the initial page stack
        let mut page_iter = self.iter_physpage().skip(pages_needed);

        //trace!("vmem_cursor: {:#018x}", vmem_cursor);
        let mut firstp = 0;

        // This will iterate across the initial page stack, setting up page
        // table entries (as needed) and writing the contents of page_iter into
        // the page stack.
        for p in self.iter_physpage().take(pages_needed) {
            //trace!("vmem_cursor: {:#018x}", vmem_cursor);
            if vmem_cursor & 0x07fffffffff == 0 {
                /* Allocate new PDPT */
                Self::map_pdpt_exact(vmem_cursor, page_iter.next().unwrap());
            }
            if vmem_cursor & 0x03fffffff == 0 {
                /* Allocate new PDE */
                let pp = page_iter.next().unwrap();
                Self::map_pde_exact(vmem_cursor, pp);
                info!("pde-mapping: {:#018x}->{:#018x}", vmem_cursor, pp);
            }
            if vmem_cursor & 0x01fffff == 0 {
                /* Allocate new PT */
                let pp = page_iter.next().unwrap();
                Self::map_pt_exact(vmem_cursor, pp);
                info!("pt-mapping: {:#018x}->{:#018x}", vmem_cursor, pp);
            }
            if vmem_cursor & 0x0fff == 0 {
                /* Map new PTE */
                Self::pinvalidate(vmem_cursor);
                Self::map_page_exact(vmem_cursor, p);
                trace!("mapping: {:#018x}->{:#018x}", vmem_cursor, p);
                Self::pinvalidate(vmem_cursor);
                for _ in 0..0x200 {
                    if let Some(np) = page_iter.next() {
                        if firstp == 0 {
                            firstp = np
                        }
                        unsafe { *(vmem_cursor as *mut usize) = np };
                        vmem_cursor += core::mem::size_of::<MemPage>();
                    }
                }
                /*trace!(
                  "End vmcursor {:#018x}",
                  vmem_cursor
                );*/
            }
        }
        let firstpg: usize = self.dynbase.into();
        info!("Old Dyn start: {:#018x}", self.dynstart.0);
        self.dynstart = ((vmem_cursor + 0xfff) & !0xfff).into();
        self.free_pages = unsafe {
            core::slice::from_raw_parts_mut(
                firstpg as *mut MemPage,
                (self.dynstart.0 - firstpg) / core::mem::size_of::<MemPage>(),
            )
        };
        info!("Done mapping");
        info!("First free page: {:#018x}", firstpg);
        info!("Free pages: {}", self.free_pages.len());
        info!("Dyn start: {:#018x}", self.dynstart.0);
    }

    fn pinvalidate(page: usize) {
        unsafe {
            asm!(
                "invlpg [{val}]",
                val = in(reg) page
            )
        }
    }

    /// Calculates the PDPT lookup from the provided vaddr
    fn pdpt_vaddr(vaddr: usize) -> usize {
        ((vaddr as isize) >> 27) as usize | 0xffffffffffe00000
    }

    /// Calculates the PDE lookup from the provided vaddr
    fn pde_vaddr(vaddr: usize) -> usize {
        ((vaddr as isize) >> 18) as usize | 0xffffffffc0000000
    }

    /// Calculates the PT lookup from the provided vaddr
    fn pt_vaddr(vaddr: usize) -> usize {
        ((vaddr as isize) >> 9) as usize | 0xffffff8000000000
    }

    fn pmap(&mut self, pages: usize) -> crate::allocator::Result<*mut u8> {
        let mapped = self.dynstart.0 as *mut u8;
        for _ in 0..pages {
            let ipdpt = Self::pdpt_vaddr(self.dynstart.into());
            let ipde = Self::pde_vaddr(self.dynstart.into());
            let ipt = Self::pt_vaddr(self.dynstart.into());
            if (self.dynstart.0 & 0x7fffffffff == 0) && Self::vtop(ipdpt).is_err() {
                /* Allocate new PDPT */
                trace!(
                    "pdpt mapping: {:#018x}->{:#018x}",
                    self.dynstart.0,
                    self.free_pages[self.ifp].0
                );
                Self::map_pdpt_exact(self.dynstart.into(), self.free_pages[self.ifp].into());
                Self::pinvalidate(ipdpt);
                self.ifp += 1;
            }
            if (self.dynstart.0 & 0x3fffffff == 0) && Self::vtop(ipde).is_err() {
                /* Allocate new PDE */
                trace!(
                    "pde mapping: {:#018x}->{:#018x}",
                    self.dynstart.0,
                    self.free_pages[self.ifp].0
                );
                Self::map_pde_exact(self.dynstart.into(), self.free_pages[self.ifp].into());
                Self::pinvalidate(ipde);
                self.ifp += 1;
            }
            if (self.dynstart.0 & 0x1fffff == 0) && Self::vtop(ipt).is_err() {
                /* Allocate new PT */
                trace!(
                    "pt mapping: {:#018x}->{:#018x}",
                    self.dynstart.0,
                    self.free_pages[self.ifp].0
                );
                Self::map_pt_exact(self.dynstart.into(), self.free_pages[self.ifp].into());
                Self::pinvalidate(ipt);
                self.ifp += 1;
            }

            /*let phys = Self::vtop(karg, self.dynstart.0 as usize).unwrap_or(0x2323232323232323);
            trace!("mapping: {:#018x}->{:#018x} (old: {:#018x})",
                self.dynstart.0,
                pgstack[self.firstpg/8 - beginning/8],
                phys
            );
            Self::pinvalidate(self.dynstart.0);
            */

            /* TODO:
             * If we are at the 0xff8 entry on the pagestack, then don't return the next
             * physical page from the stack. Instead, free the following page and remap
             * its physical page instead.
             */
            Self::map_page_exact(self.dynstart.into(), self.free_pages[self.ifp].into());
            Self::pinvalidate(self.dynstart.into());
            self.ifp += 1;
            self.dynstart = MemPage(self.dynstart.0 + 0x1000);
        }
        Ok(mapped)
    }

    /// Unmaps the vmem page and places it back on the stack. Returns the
    /// address of the physical page placed on the stack. None is returned
    /// if it couldn't unmap the page.
    pub fn pmap_free(&mut self, page: usize) -> crate::allocator::Result<usize> {
        let phys = Self::vtop(page)?;
        // TODO: Similar to palloc, implement logic when we cross a page boundary
        // with the head of the page stack, then the freed physical page needs to
        // be mapped to extend the capacity of the page stack.

        // Unmap the page
        Self::map_page_exact(page, 0);

        // Put the physical page back on the page stack
        self.free_pages[self.ifp - 1] = phys.into();
        Ok(phys)
    }

    /// Initialize the recursive paging structure for vtop lookup and
    /// easy remapping
    fn init_recursive(&mut self) {
        let pml4_ptr = core::ptr::from_mut(self.pml4) as *mut PDEntry;
        self.pml4[511] = PDEntry::from_paddr(pml4_ptr as usize);
    }

    /// Fetch the PML4 physical memory pointer from CR3
    fn get_pml4() -> &'static mut [PDEntry] {
        let pml4_ptr: *mut PDEntry; //*mut u64;
        unsafe {
            asm!(
                "mov {val}, cr3",
                val = out(reg) pml4_ptr,
            )
        }
        let pml4 =
            unsafe { from_raw_parts_mut(pml4_ptr, 0x1000 / core::mem::size_of::<PDEntry>()) };

        /* Update the last entry in the PML4 */
        //pml4[511] = PDEntry::from_paddr(pml4_ptr as usize);
        pml4
    }

    /// Translate virtual to physical addresses using the CPU paging structures
    pub fn vtop(inptr: usize) -> crate::allocator::Result<usize> {
        /* Sign extension is performed when shifting signed values, so convert
         * addresses to isize for the shift, then back to usize afterward.
         */
        let ptlookup = ((inptr as isize & !0xfff) >> 9) as usize | 0xffffff8000000000;

        /* Make sure that we recursively test the validity of the page map level
         * pointers on the way to retrieving the physical address mapping. This
         * guards against a page fault when navigating the paging structures due
         * to an intermediate lookup table not being mapped at all.
         */
        if ptlookup < 0xfffffffffffff000 && Self::vtop(ptlookup).is_err() {
            Err(crate::allocator::Error::UnmappedPage {
                page: inptr,
                pt: ptlookup,
            })
        } else {
            unsafe { (ptlookup as *const usize).as_ref() }
                .filter(|x| (*x & 0x1) == 1) // If !Present, then return Err
                .ok_or(crate::allocator::Error::UnmappedPage {
                    page: inptr,
                    pt: ptlookup,
                })
                .map(|x| *x & 0xfffffffffffff000 | (inptr & 0xfff))
        }
    }

    /// Generic paging structure mapping utility function. Will accept a physical address
    /// and a virtual address, and will implement the requested shift and appropriate
    /// mapping for the paging structure level the shift is requesting the map be
    /// performed on.
    fn map_any_exact(vaddr: usize, paddr: usize, shift: usize) {
        /* Sign extension is performed when shifting signed values, so convert
         * addresses to isize for the shift, then back to usize afterward.
         */
        let ptlookup = ((vaddr as isize) >> shift) as usize;
        let page = unsafe { (ptlookup as *mut PDEntry).as_mut() };
        if let Some(p) = page {
            if paddr > 0 {
                *p = crate::paging::PDEntry::from_paddr(paddr);
                Self::pinvalidate(ptlookup as usize);

                // Zero out the allocated page (only if mapping a phys page)
                let pdlookup = ((vaddr as isize) >> (shift - 9)) as usize;
                let pdpage = unsafe {
                    core::slice::from_raw_parts_mut((pdlookup & !0xfff) as *mut PDEntry, 0x200)
                };
                if shift > 9 {
                    pdpage.fill(crate::paging::PDEntry::new_null());
                }
                Self::pinvalidate(pdlookup as usize);
            } else {
                *p = crate::paging::PDEntry::new_null();
                Self::pinvalidate(ptlookup as usize);
            }
        }
    }

    /// Maps a specific vaddr PTE to a paddr. Expects the PML4, PDPT, PDE,
    /// and PT to all be existing.
    fn map_page_exact(vaddr: usize, paddr: usize) {
        Self::map_any_exact(vaddr, paddr, 9)
    }

    /// Maps a specific PT to a paddr. Expects the PML4, PDPT, PDE,
    /// to all already be existing
    fn map_pt_exact(vaddr: usize, paddr: usize) {
        Self::map_any_exact(vaddr, paddr, 18)
    }

    /// Maps a specific PDE to a paddr. Expects the PML4 & PDPT
    /// to both already be existing
    fn map_pde_exact(vaddr: usize, paddr: usize) {
        Self::map_any_exact(vaddr, paddr, 27)
    }

    /// Maps a specific PDPT to a paddr. Expects the PML4
    /// to already be existing
    fn map_pdpt_exact(vaddr: usize, paddr: usize) {
        Self::map_any_exact(vaddr, paddr, 36)
    }

    /// Given a physical and canonical virtual page address, will map the
    /// physical page to the requested virtual page, and will map the higher
    /// level structures where necessary. Will return an error if another
    /// page is already mapped there.
    pub fn map_vtop(&mut self, vaddr: usize, paddr: usize) -> Result<()> {
        if let Ok(p) = Self::vtop(vaddr) {
            Err(Error::MapCollision { vpage: vaddr, ppage: paddr })
        } else {
            // If we get here, then we assume it is safe to map any missing
            // page levels, and the target page can be mapped
            let ipdpt = Self::pdpt_vaddr(vaddr);
            let ipde = Self::pde_vaddr(vaddr);
            let ipt = Self::pt_vaddr(vaddr);

            // To be on the safe side, we will throw an "insufficient free" error if
            // the available free pages is less than 4 - which would prevent us from
            // mapping the page plus all intermediate paging structures, if all are
            // unmapped
            if self.free_pages.len() - self.ifp < 4 {
                Err(crate::allocator::Error::InsufficientFree)?
            }

            // If the associated PDPT is empty, need to map a new one
            if Self::vtop(ipdpt).is_err() {
                let next_free = self.free_pages[self.ifp].into();
                Self::map_pdpt_exact(vaddr, next_free);
                Self::pinvalidate(ipdpt);
                self.ifp += 1;
            }

            // If the associated PDE is empty, need to map a new one
            if Self::vtop(ipde).is_err() {
                let next_free = self.free_pages[self.ifp].into();
                Self::map_pde_exact(vaddr, next_free);
                Self::pinvalidate(ipde);
                self.ifp += 1;
            }

            // If the associated PT is empty, need to map a new one
            if Self::vtop(ipt).is_err() {
                let next_free = self.free_pages[self.ifp].into();
                Self::map_pt_exact(vaddr, next_free);
                Self::pinvalidate(ipt);
                self.ifp += 1;
            }

            // Finally, map the requested phys page to its appropriate vpage in
            // the (possibly new) PT
            Self::map_page_exact(vaddr & !0xfff, paddr & !0xfff);
            Self::pinvalidate(vaddr & !0xfff);

            Ok(())
        }
    }
}

pub static mut MEM_DRIVER: MemDriver = MemDriver {
    pml4: &mut [],
    dynbase: MemPage(0),
    dynstart: MemPage(0),
    memmap: SysMemMap { memmap: &[] },
    free_pages: &mut [],
    ifp: 0,
};
