use core::alloc::{GlobalAlloc, Layout, LayoutError};
use log::{error, info, trace};
use snafu::prelude::*;

/// Minimum segment allocation size is 256kB (64 pages)
const MIN_SEGMENT_SIZE: usize = 64;

/// Minimum free block size (in bytes) we will allow before
/// we just assume that a free block is too small to track
/// in a free list
const MIN_FREE_SIZE: usize = core::mem::size_of::<KernFree>();

#[derive(Debug, Snafu)]
pub(crate) enum Error {
    /// Page allocator not defined
    NoPageAllocator,

    /// An invalid page was returned by the PageAllocator
    InvalidPageReturned,

    /// Not enough free pages remain to satisfy allocation request
    InsufficientFree,

    /// Allocation failure unrelated to free page availability
    AllocationFailed,

    /// Address provided by caller was in a page that wasn't mapped: page: {page:#018x},
    /// pt: {pt:#018x}
    UnmappedPage { page: usize, pt: usize },

    /// Error with Layout
    Layout,
}

pub(crate) type Result<T> = core::result::Result<T, Error>;

/// Trait to describe implementation of the PageAllocator.
/// Defines an object that the caller can request new pages
/// from, and will also free/reclaim previously-allocated pages.
pub trait PageAllocator {
    /// Allocate the number of pages in `pages` and will
    /// return an Option with a char* pointer to the new
    /// buffer, or None if no allocation was possible.
    fn palloc(&mut self, pages: usize) -> Result<*mut u8>;

    /// Will attempt to free the `count` pages pointed at by `ptr`.
    /// If no freeing action is taken, will silently continue.
    fn pfree(&mut self, ptr: *mut u8, count: usize) -> Result<()>;
}

#[derive(Clone, Debug)]
//#[repr(C)]
struct KernFree {
    /// The base address of this available run of memory
    buffer: *mut u8,

    /// The length of this free segment of memory
    length: usize,

    /// The next free chunk in the list (None if terminal)
    next: Option<*mut KernFree>,
}

#[derive(Debug)]
struct KernFreeIter {
    cur: Option<*mut KernFree>,
}

impl KernFree {
    fn iter(&mut self) -> KernFreeIter {
        KernFreeIter {
            cur: Some(core::ptr::from_mut(self)),
        }
    }

    /// Attempts to return an allocation, and an optional KernFree remainder, from the
    /// current free block.
    fn alloc_from(&mut self, layout: Layout) -> (Option<KernAllocation>, Option<&mut Self>) {
        let my_buffer = self.buffer;
        trace!(
            "self-ptr: {:p}, self-length: {}, demand: {}, align: {}, min_free: {}, buffer: {:#018x}, next: {:#018x}",
            self,
            self.length,
            layout.pad_to_align().size(),
            layout.align(),
            MIN_FREE_SIZE,
            my_buffer as usize,
            self.next.map(|x| x as usize).unwrap_or(0xd0d0d0d0d0d0d0d0)
        );
        if self.length >= layout.pad_to_align().size() + MIN_FREE_SIZE {
            // If the freeblock is big enough to hold the allocation and also
            // track another smaller free block, then we need to partition
            // and return the remainder
            let offset = (((self.buffer as usize) + layout.align() - 1) & !(layout.align() - 1))
                - (self.buffer as usize);
            let nfp_offset = ((((self.buffer as usize) + layout.pad_to_align().size()) + 7) & !7) - (self.buffer as usize);
            let new_free_ptr = unsafe { self.buffer.add(nfp_offset) };
            if new_free_ptr.is_null() {
                panic!("buffer was null! nfp-{:#018x} b-{:#018x}", new_free_ptr as usize, my_buffer as usize);
                return (None, Some(self))
            }
            let new_free = unsafe { (new_free_ptr as *mut KernFree).as_mut().unwrap() };
            (*new_free) = KernFree {
                buffer: new_free_ptr,
                length: self.length - nfp_offset,
                next: self.next,
            };
            trace!("nfp: {:?}, nf: {:p}", new_free_ptr, new_free);
            if (*new_free).buffer as usize == 0xff00000000000000 {
                panic!("buffer was bad!")
            }
            (
                Some(KernAllocation {
                    buffer: (my_buffer as usize + offset) as *mut u8,
                    length: layout.pad_to_align().size(),
                }),
                Some(new_free),
            )
        } else if self.length >= layout.pad_to_align().size() {
            // If the freeblock is big enough for the allocation, but not a
            // remainder freeblock, then just return the new allocation and
            // discard the remainder.
            let offset = (((self.buffer as usize) + layout.align() - 1) & !(layout.align() - 1))
                - (self.buffer as usize);
            (
                Some(KernAllocation {
                    buffer: (my_buffer as usize + offset) as *mut u8,
                    length: layout.pad_to_align().size() - offset,
                }),
                None,
            )
        } else {
            // If this free block isn't big enough to store the demanded buffer,
            // then skip to next and return self
            (None, Some(self))
        }
    }
}

impl Iterator for KernFreeIter {
    type Item = *mut KernFree;

    fn next(&mut self) -> Option<Self::Item> {
        self.cur.take().and_then(|x| {
            let xp = x;
            self.cur = unsafe { x.as_mut().unwrap() }.next;
            Some(xp)
        })
    }
}

#[derive(Copy, Clone, Debug)]
struct KernAllocation {
    /// The base address of the buffer this allocation
    /// manages
    buffer: *mut u8,

    /// The length of the buffer managed by this allocation
    length: usize,
}

impl Default for KernAllocation {
    fn default() -> Self {
        Self {
            buffer: core::ptr::null_mut(),
            length: 0,
        }
    }
}

#[derive(Debug)]
struct KernHeapSegment {
    /// Base address of the managed memory segment
    base_addr: usize,

    /// Length of the managed memory segment
    length: usize,

    /// The allocation hashtable
    alloc_table: &'static mut [KernAllocation],

    /// The list of available "free" chunks within the managed
    /// memory segment
    freelist: Option<*mut KernFree>,

    /// Pointer to the next segment in the list (None if terminal)
    next: Option<*mut KernHeapSegment>,
}

impl KernHeapSegment {
    fn iter(&mut self) -> KernHeapSegmentIter {
        KernHeapSegmentIter { cur: Some(self) }
    }
}

struct KernHeapSegmentIter {
    cur: Option<*mut KernHeapSegment>,
}

impl Iterator for KernHeapSegmentIter {
    type Item = *mut KernHeapSegment;

    fn next(&mut self) -> Option<Self::Item> {
        self.cur.take().and_then(|x| {
            let xp = x;
            self.cur = unsafe { x.as_mut().unwrap() }.next;
            Some(xp)
        })
    }
}

#[derive(Debug)]
pub struct KernelAlloc {}

struct InnerKernelAlloc {
    page_allocator: Option<&'static mut (dyn PageAllocator + Sync)>,
    kheap: *mut KernHeapSegment,
}

impl core::fmt::Debug for InnerKernelAlloc {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::result::Result<(), core::fmt::Error> {
        match &self.page_allocator {
            Some(pa) => {
                write!(
                    f,
                    "InnerKernelAlloc{{ page_allocator: {:?}, kheap: {:?} }}",
                    core::ptr::from_ref(pa) as *mut u8,
                    self.kheap
                )
            }
            None => {
                write!(f, "InnerKernelAlloc{{ page_allocator: None, kheap: {:?} }}", self.kheap)
            }
        }
    }
}

impl Default for InnerKernelAlloc {
    fn default() -> InnerKernelAlloc {
        InnerKernelAlloc {
            page_allocator: None,
            kheap: core::ptr::null_mut(),
        }
    }
}

impl KernelAlloc {
    pub fn set_page_allocator(&self, palloc: *mut (dyn PageAllocator + Sync + 'static)) {
        unsafe { INNER_KERN_ALLOC.set_page_allocator(palloc) };
    }
}

impl InnerKernelAlloc {
    pub fn set_page_allocator(&mut self, palloc: *mut (dyn PageAllocator + Sync + 'static)) {
        self.page_allocator = unsafe { palloc.as_mut() };
    }

    /// Extend the heap by a given number of pages by creating a
    /// new segment to add to the end of the heap. This is used when the alloc_table
    /// of existing segments are full, or when none of the existing segments have
    /// enough contiguous free space for allocating a buffer being requested. Will
    /// return the new segment so that it can be inserted without recursing the
    /// segment list again.
    fn extend_heap(&mut self, pages: usize) -> Result<*mut KernHeapSegment> {
        let pa = &mut self
            .page_allocator
            .as_deref_mut()
            .ok_or(Error::NoPageAllocator)?;
        // Allocate the pages, plus one for the management structure
        let seg = pa.palloc(1 + pages).and_then(|s| {
            unsafe { (s as *mut KernHeapSegment).as_mut() }.ok_or(Error::InvalidPageReturned)
        })?;
        let block_start: *mut KernFree =
            ((core::ptr::from_ref(seg) as usize) + 0x1000) as *mut KernFree;

        // The management structure contains the base address of the managed page(s),
        // length of the managed segment (in pages), a table of the allocations within
        // this segment, and a `next` pointer linking to the next segment
        *seg = KernHeapSegment {
            base_addr: core::ptr::from_ref(seg) as usize,
            length: pages,
            alloc_table: unsafe {
                core::slice::from_raw_parts_mut(
                    ((core::ptr::from_ref(seg) as usize
                        + core::mem::size_of::<KernHeapSegment>()
                        + 0xf)
                        & !0x0f) as *mut KernAllocation,
                    0x1000 - ((core::mem::size_of::<KernHeapSegment>() + 0xf) & !0x0f),
                )
            },
            freelist: Some(block_start),
            next: None,
        };

        // Fill the .alloc_table with empty entries
        (*seg).alloc_table.fill(KernAllocation {
            buffer: core::ptr::null_mut::<u8>(),
            length: 0,
        });

        // Initialize the data area as a single "free" block of memory. The KernFree
        // structure is actually stored inside of the free block that it represents,
        // enabling us to manage free memory without additional overhead.
        let fl = &mut (*seg).freelist.unwrap();
        unsafe {
            fl.write(KernFree {
                buffer: *fl as *mut u8,
                length: 0x1000 * pages,
                next: None,
            });
            if *fl as usize == 0xff00000000000000 {
                panic!("buffer was bad!")
            };
        };

        // Return success Option
        Ok(seg)
    }

    unsafe fn alloc(&mut self, layout: Layout) -> Result<*mut u8> {
        // Do an unsafe const -> mut conversion here because GlobalAlloc
        // trait uses const instead of mut for &self
        //let mutself = (core::ptr::from_ref(self) as *mut Self).as_mut()?;
        let adj_layout = if layout.align() < 8 {
            //Layout::from_size_align(layout.size(), 8).map_err(|_| Error::Layout)?
            layout
        } else {
            layout
        };
        let nss = if adj_layout.size() > MIN_SEGMENT_SIZE {
            adj_layout.size()
        } else {
            MIN_SEGMENT_SIZE
        };

        if self.kheap.is_null() {
            // If the heap's completely empty, then initialize it with
            // a new segment
            //let new_seg = (*mutselfptr).extend_heap(nss)?;
            self.kheap = self.extend_heap(nss)?;
            //trace!("Extended heap: {:?}", sel);
            //trace!("Extended heap: {:?}", self.kheap);
        };

        // At this point, kheap is guaranteed to be at least one segment
        // long
        let kh = self.kheap.as_mut().unwrap();
        for s in kh.iter() {
            // TODO: need to solve the problems of how to get the prev freelist entry, when
            // the entry containing a suitable buffer is encountered. The free entry
            // containing sufficient memory needs to be partitioned, and the prev free entry
            // needs to be able to point at the new (smaller) free entry, or the following
            // one, if not enough space left.
            //
            // May just have to exercise a special test-case when trying the head of the
            // freelist, to deal with that we'll need to edit the segment's freelist reference,
            // rather than a freelist member's next value.

            // If no freelist, move on
            if (*s).freelist.is_none() {
                continue;
            }

            let flptr = (*s).freelist.unwrap();
            trace!("flptr: {:?}", flptr);
            trace!("flptrbuf: {:?}", (*flptr).buffer);

            // If head of freelist can't be used, then scan for another node
            // that can be used
            let mut fiter = flptr.as_mut().unwrap().iter().peekable();

            // See if there's an available free slot on the heap
            let m = (*s).alloc_table.iter_mut().find(|x| x.length == 0);
            if m.is_some() {
                trace!("flhead: {:?}", (**(fiter.peek().unwrap())).buffer);
                // See if the head of the freelist can be used
                match (*(*s).freelist.unwrap()).alloc_from(adj_layout) {
                    (Some(ka), Some(kf)) => {
                        (*s).freelist = Some(kf);
                        *(m.unwrap()) = ka;
                        trace!("KernAlloc1(ka+kf): {:?} {:?}", ka, kf);
                        trace!("freelist: {:?}",(*s).freelist);
                        trace!("flbuf: {:#018x}", (*(*s).freelist.unwrap()).buffer as usize);
                        if (kf.buffer as usize) == 0x0000000000000000 {
                            panic!("Bad value for Kfree!!!")
                        }
                        return Ok(ka.buffer);
                    }
                    (Some(ka), None) => {
                        (*s).freelist = (*flptr).next;
                        *(m.unwrap()) = ka;
                        trace!("KernAlloc1(ka): {:?}", ka);
                        return Ok(ka.buffer);
                    }
                    _ => {
                        trace!("KernAlloc1(other)");
                    }
                };

                // Scan across fiter until a suitable freelist block is found that meets our
                // sizing needs (or we reach the end)
                while let Some(_) =
                    fiter.next_if(|&x| {
                        x.as_ref().unwrap().next.as_ref().is_some_and(|&y| {
                            y.as_ref().unwrap().length < adj_layout.pad_to_align().size()
                        })
                    })
                {}

                // If fiter.peek_mut() yields a Some() then it means we found a match and we
                // should be able to alloc in this free block, so long as a free allocation
                // slot is open
                if let Some(&mut following) = fiter.peek_mut() {
                    let m = (*s).alloc_table.iter_mut().find(|x| x.length == 0);
                    let next = fiter.next().unwrap().as_mut().unwrap();
                    match following.as_mut().unwrap().alloc_from(adj_layout) {
                        (Some(ka), Some(kf)) => {
                            next.next = Some(kf);
                            *(m.unwrap()) = ka;
                            trace!("KernAlloc2(ka+kf): {:?} {:?}", ka, kf);
                            if (kf.buffer as usize) == 0xff00000000000000 {
                                panic!("Bad value for Kfree!!!")
                            }
                            return Ok(ka.buffer);
                        }
                        (Some(ka), None) => {
                            next.next = Some(following);
                            *(m.unwrap()) = ka;
                            trace!("KernAlloc2(ka): {:?}", ka);
                            return Ok(ka.buffer);
                        }
                        _ => {
                            trace!("KernAlloc2(other)");
                        }
                    };
                }
            };

            // If we get this far it is because we haven't found any free space in the entire
            // list of segments. Therefore, it is possibly time to page in another segment.
            if (*s).next == None {
                (*s).next = Some(self.extend_heap(nss)?);
                trace!("KernAlloc recursing");
                return self.alloc(adj_layout);
            }
        }

        // When we get here it's because there was some Allocation failure. If there's no
        // free mem pages left, then that should have been reported/caught during one of
        // the extend_heap() calls above
        Err(Error::AllocationFailed)
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8) {
        trace!(
            "Dealloc: {:#018x}",
            ptr as usize
        );
        for s in (*(self.kheap)).iter() {
            if ((*s).base_addr + ((*s).length * 0x1000)) < (ptr as usize) {
                // If the segment's tail is numerically before ptr, then we need to
                // advance to the next segment
                trace!(
                    "Skip: {:#018x}-{:#018x} < {:#018x}",
                    (*s).base_addr,
                    (*s).length * 0x1000,
                    ptr as usize
                );
                continue;
            } else if (*s).base_addr < (ptr as usize) {
                // If base_addr is less than ptr here, it means that ptr is
                // contained somewhere in this segment, because the tail
                // is numerically after ptr, while the head is numerically
                // before it. So this is where the free has to happen

                trace!(
                    "Working: {:#018x}-{:#018x} < {:#018x}",
                    (*s).base_addr,
                    (*s).length * 0x1000,
                    ptr as usize
                );

                for a in (*s).alloc_table.iter_mut() {
                    if a.buffer as *mut u8 == ptr {
                        // Find and remove the item from alloc_table
                        let b = *a;
                        *a = KernAllocation::default();

                        if b.length < core::mem::size_of::<KernFree>() {
                            trace!("Freed block (len={}) less than KernFree (len={})", b.length, core::mem::size_of::<KernFree>());
                            return;
                        }

                        // Then add it back to the free list
                        if let Some(fl) = (*s).freelist {
                            // If it belongs at the head, insert it at the head
                            if (b.buffer as usize) < ((*fl).buffer as usize) {
                                let mut freed = KernFree {
                                    buffer: b.buffer,
                                    length: b.length,
                                    next: Some(fl),
                                };

                                if (freed.buffer as usize) == 0xff00000000000000 {
                                    panic!("Bad value for Kfree!!!")
                                }

                                // If the successive entries in the freelist are adjacent blocks,
                                // then recombine them
                                while freed.next.is_some_and(|x| {
                                    ((*x).buffer as usize) == (freed.buffer as usize + freed.length)
                                }) {
                                    trace!("Freed(1): {:?}", freed);
                                    freed.length += (*(freed.next.unwrap())).length;
                                    freed.next = (*(freed.next.unwrap())).next;
                                }

                                let new_free = b.buffer as *mut KernFree;
                                if (new_free as usize) == 0xff00000000000000 {
                                    panic!("Bad value for Kfree!!!")
                                }

                                *new_free = freed;
                                (*s).freelist = Some(new_free);
                                trace!("NewFree (head ptr ): {:#018x}", new_free as usize);
                                trace!("NewFree (s    ptr ): {:#018x}", s as usize);
                                trace!("NewFree (head cont): {:?}", *new_free);
                                trace!("NewFree (size of  ): {:?}", core::mem::size_of::<Option<KernFree>>());
                                if (new_free as usize) == 0xff00000000000000 {
                                    panic!("Bad value for Kfree!!!")
                                }
                                return;
                            }

                            let mut fli = (*fl).iter();
                            loop {
                                let freeblockptr = fli.next().unwrap();
                                let fb = freeblockptr.as_mut().unwrap();

                                // If this freeblock ends where the just-freed chunk begins,
                                // then merge them
                                if (fb.buffer as usize) + fb.length == (b.buffer as usize) {
                                    fb.length += b.length;
                                    // break;
                                    trace!("NewFree (ext): {:?}", *fb);
                                    return;
                                }

                                // If the next block isn't present, then this newly freed
                                // block goes on the end
                                if fb.next.is_none() {
                                    let freed = KernFree {
                                        buffer: b.buffer,
                                        length: b.length,
                                        next: None,
                                    };

                                    if (freed.buffer as usize) == 0xff00000000000000 {
                                        panic!("Bad value for Kfree!!!")
                                    }

                                    let new_free = b.buffer as *mut KernFree;
                                    *new_free = freed;
                                    fb.next = Some(new_free);

                                    if (new_free as usize) == 0xff00000000000000 {
                                        panic!("Bad value for Kfree!!!")
                                    }

                                    // Guarantees that fli.next never yields a None
                                    trace!("NewFree (tail): {:?}", *new_free);
                                    return;
                                }

                                // If the next block is present, and it is numerically past the
                                // newly-freed block, then insert the newly-freed block between
                                // them
                                let fbn = fb.next.unwrap().as_mut().unwrap();
                                if (b.buffer as usize) + b.length < (fbn.buffer as usize) {
                                    let mut freed = KernFree {
                                        buffer: b.buffer,
                                        length: b.length,
                                        next: fb.next,
                                    };

                                    if (freed.buffer as usize) == 0xff00000000000000 {
                                        panic!("Bad value for Kfree!!!")
                                    }

                                    while freed.next.is_some_and(|x| {
                                        ((*x).buffer as usize)
                                            == (freed.buffer as usize + freed.length)
                                    }) {
                                        trace!("Freed(2): {:?}", freed);
                                        freed.length += (*(freed.next.unwrap())).length;
                                        freed.next = (*(freed.next.unwrap())).next;
                                    }

                                    let new_free = b.buffer as *mut KernFree;
                                    *new_free = freed;

                                    if (new_free as usize) == 0xff00000000000000 {
                                        panic!("Bad value for Kfree!!!")
                                    }

                                    fb.next = Some(new_free);
                                    trace!("NewFree (insert): {:?}", *new_free);
                                    return;
                                }

                                // Otherwise, just keep looping and scanning across the iterator
                            }
                        } else {
                            // Free-list is empty so just populate it with the
                            // current block being deallocated
                            let freed = KernFree {
                                buffer: a.buffer,
                                length: a.length,
                                next: None,
                            };


                            if (freed.buffer as usize) == 0xff00000000000000 {
                                panic!("Bad value for Kfree!!!")
                            }
                            let new_free = a.buffer as *mut KernFree;
                            *new_free = freed;

                            if (new_free as usize) == 0xff00000000000000 {
                                panic!("Bad value for Kfree!!!")
                            }
                            (*s).freelist = Some(new_free);
                            trace!("NewFree (lone): {:?}", *new_free);
                            return;
                        }
                    }
                }
            } else {
                // We ended up past where this pointer belongs and don't know wtf to do
                break;
            }

            // When we arrive here, it either means we didn't find the ptr
            // or we did find it and we just freed it
            return;
        }
    }
}

unsafe impl GlobalAlloc for KernelAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        INNER_KERN_ALLOC
            .alloc(layout)
            .unwrap_or(core::ptr::null::<u8>() as *mut u8)
    }
    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        INNER_KERN_ALLOC.dealloc(ptr)
    }
}

static mut INNER_KERN_ALLOC: InnerKernelAlloc = InnerKernelAlloc {
    page_allocator: None,
    kheap: core::ptr::null_mut(),
};

#[global_allocator]
pub static KERN_ALLOC: KernelAlloc = KernelAlloc {};
