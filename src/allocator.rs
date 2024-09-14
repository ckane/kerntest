use core::alloc::{GlobalAlloc, Layout};
use log::{error, info, trace};

/// Minimum segment allocation size is 256kB (64 pages)
const MIN_SEGMENT_SIZE: usize = 64;

/// Minimum free block size (in bytes) we will allow before
/// we just assume that a free block is too small to track
/// in a free list
const MIN_FREE_SIZE: usize = core::mem::size_of::<KernFree>();

/// Trait to describe implementation of the PageAllocator.
/// Defines an object that the caller can request new pages
/// from, and will also free/reclaim previously-allocated pages.
pub trait PageAllocator {
    /// Allocate the number of pages in `pages` and will
    /// return an Option with a char* pointer to the new
    /// buffer, or None if no allocation was possible.
    fn palloc(&mut self, pages: usize) -> Option<*mut u8>;

    /// Will attempt to free the `count` pages pointed at by `ptr`.
    /// If no freeing action is taken, will silently continue.
    fn pfree(&mut self, ptr: *mut u8, count: usize);
}

#[derive(Clone, Debug)]
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
        trace!(
            "self-length: {}, demand: {}, min_free: {}",
            self.length,
            layout.pad_to_align().size(),
            MIN_FREE_SIZE
        );
        if self.length >= layout.pad_to_align().size() + MIN_FREE_SIZE {
            // If the freeblock is big enough to hold the allocation and also
            // track another smaller free block, then we need to partition
            // and return the remainder
            let new_free_ptr = unsafe { self.buffer.add(layout.pad_to_align().size()) };
            let new_free = unsafe { (new_free_ptr as *mut KernFree).as_mut().unwrap() };
            (*new_free) = KernFree {
                buffer: new_free_ptr,
                length: self.length - layout.pad_to_align().size(),
                next: self.next,
            };
            (
                Some(KernAllocation {
                    buffer: self.buffer,
                    length: layout.pad_to_align().size(),
                }),
                Some(new_free),
            )
        } else if self.length >= layout.pad_to_align().size() {
            // If the freeblock is big enough for the allocation, but not a
            // remainder freeblock, then just return the new allocation and
            // discard the remainder.
            (
                Some(KernAllocation {
                    buffer: self.buffer,
                    length: self.length,
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
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> Result<(), core::fmt::Error> {
        match &self.page_allocator {
            Some(pa) => {
                write!(
                    f,
                    "page_allocator: {:?}, kheap: {:?}\n",
                    core::ptr::from_ref(pa) as *mut u8,
                    self.kheap
                )
            }
            None => {
                write!(f, "page_allocator: None, kheap: {:?}\n", self.kheap)
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
    fn extend_heap(&mut self, pages: usize) -> Option<*mut KernHeapSegment> {
        let pa = &mut self.page_allocator.as_deref_mut()?;
        // Allocate the pages, plus one for the management structure
        let seg = pa
            .palloc(1 + pages)
            .and_then(|s| unsafe { (s as *mut KernHeapSegment).as_mut() })?;
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
            })
        };

        // Return success Option
        Some(seg)
    }

    unsafe fn alloc(&mut self, layout: Layout) -> Option<*mut u8> {
        // Do an unsafe const -> mut conversion here because GlobalAlloc
        // trait uses const instead of mut for &self
        //let mutself = (core::ptr::from_ref(self) as *mut Self).as_mut()?;
        let nss = if layout.size() > MIN_SEGMENT_SIZE {
            layout.size()
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

            // If head of freelist can't be used, then scan for another node
            // that can be used
            let mut fiter = flptr.as_mut().unwrap().iter().peekable();

            // See if there's an available free slot on the heap
            let m = (*s).alloc_table.iter_mut().find(|x| x.length == 0);
            if m.is_some() {
                // See if the head of the freelist can be used
                match (*(*s).freelist.unwrap()).alloc_from(layout) {
                    (Some(ka), Some(kf)) => {
                        (*s).freelist = Some(kf);
                        *(m.unwrap()) = ka;
                        trace!("KernAlloc(ka+kf): {:?} {:?}", ka, kf);
                        return Some(ka.buffer);
                    }
                    (Some(ka), None) => {
                        (*s).freelist = (*flptr).next;
                        *(m.unwrap()) = ka;
                        trace!("KernAlloc(ka): {:?}", ka);
                        return Some(ka.buffer);
                    }
                    _ => {
                        trace!("KernAlloc(other)");
                    }
                };

                // Scan across fiter until a suitable freelist block is found that meets our
                // sizing needs (or we reach the end)
                while let Some(_) =
                    fiter.next_if(|&x| {
                        x.as_ref().unwrap().next.as_ref().is_some_and(|&y| {
                            y.as_ref().unwrap().length < layout.pad_to_align().size()
                        })
                    })
                {}

                // If fiter.peek_mut() yields a Some() then it means we found a match and we
                // should be able to alloc in this free block, so long as a free allocation
                // slot is open
                if let Some(&mut following) = fiter.peek_mut() {
                    let m = (*s).alloc_table.iter_mut().find(|x| x.length == 0);
                    let next = fiter.next().unwrap().as_mut().unwrap();
                    match following.as_mut().unwrap().alloc_from(layout) {
                        (Some(ka), Some(kf)) => {
                            next.next = Some(kf);
                            *(m.unwrap()) = ka;
                            return Some(ka.buffer);
                        }
                        (Some(ka), None) => {
                            next.next = Some(following);
                            *(m.unwrap()) = ka;
                            return Some(ka.buffer);
                        }
                        _ => {}
                    };
                }
            };

            // If we get this far it is because we haven't found any free space in the entire
            // list of segments. Therefore, it is possibly time to page in another segment.
            if (*s).next == None {
                (*s).next = self.extend_heap(nss);
                trace!("KernAlloc recursing");
                return self.alloc(layout);
            }
        }
        None
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8) {
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

                        // Then add it back to the free list
                        if let Some(fl) = (*s).freelist {
                            // If it belongs at the head, insert it at the head
                            if (b.buffer as usize) < ((*fl).buffer as usize) {
                                let mut freed = KernFree {
                                    buffer: b.buffer,
                                    length: b.length,
                                    next: Some(fl),
                                };

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
                                *new_free = freed;
                                (*s).freelist = Some(new_free);
                                trace!("NewFree (head): {:?}", *new_free);
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

                                    let new_free = b.buffer as *mut KernFree;
                                    *new_free = freed;
                                    fb.next = Some(new_free);

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

                            let new_free = a.buffer as *mut KernFree;
                            *new_free = freed;
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
pub static mut KERN_ALLOC: KernelAlloc = KernelAlloc {};
