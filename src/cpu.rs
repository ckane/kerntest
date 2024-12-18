use crate::thread::{Thread, ThreadState};
use alloc::boxed::Box;
use alloc::collections::vec_deque::VecDeque;
use alloc::vec::Vec;
use core::arch::asm;
use core::ops::Drop;
use core::sync::atomic::{AtomicUsize, Ordering};

#[derive(Clone, Debug)]
pub(crate) struct Processor {
    id: u64,
    runq: VecDeque<Box<Thread>>,
    active: Option<Box<Thread>>,
    crit_stack: usize,
}

impl Processor {
    pub fn new(id: u64) -> Self {
        Self {
            id,
            runq: VecDeque::new(),
            active: None,
            crit_stack: 1,
        }
    }

    fn preempt_thread(&mut self) {
        // self.runq.push_back(self.active.unwrap().clone());
    }

    pub(crate) fn kill_active_thread(&mut self) {
        self.active.take();
    }

    pub(crate) fn active_thread(&self) -> Option<&Thread> {
        self.active.as_deref()
    }
}

pub fn add_cpu() {
    unsafe {
        CPUS.push(Processor::new(CPUS.len() as u64));
    }
}

pub fn add_thread(id: u64, mut th: Box<Thread>) {
    unsafe {
        th.set_queued();
        CPUS[id as usize].runq.push_back(th);
    }
}

pub fn get_cpu(id: usize) -> Option<&'static Processor> {
    unsafe { CPUS.get(id) }
}

pub fn get_cpu_mut(id: usize) -> Option<&'static mut Processor> {
    unsafe { CPUS.get_mut(id) }
}

/// Save the current context
pub fn save_context(cpu: u64, context: crate::thread::ThreadContext) {
    // Only save the context if an existing Thread is marked active, otherwise
    // just discard the context.
    unsafe {
        let cur_thread = CPUS[cpu as usize].active.as_mut();
        if let Some(thread) = cur_thread {
            thread.save_context(context);
        }
    }
}

pub fn preempt_thread(cpu: u64) -> crate::thread::ThreadContext {
    unsafe {
        if let Some(cpu_ref) = CPUS.get_mut(cpu as usize) {
            // Move the running task to the back of the runq
            if let Some(mut th) = cpu_ref.active.take() {
                th.set_queued();
                cpu_ref.runq.push_back(th);
            }

            // Pop the next item off the runq and run it
            let mut next_thread = cpu_ref.runq.pop_front().unwrap();
            next_thread.set_running();
            let context = next_thread.get_context();
            cpu_ref.active = Some(next_thread);
            cpu_ref.crit_stack = 0;
            context
        } else {
            crate::thread::ThreadContext::default()
        }
    }
}

pub(crate) fn get_core_id() -> u64 {
    let mut core_id = 0xd0d0d0d0d0d0d0d0u64;
    unsafe {
        asm!(
            "rdtscp",
            "mov {}, rcx",
            out(reg) core_id
        );
    };
    core_id
}

pub(crate) struct CritSection;

impl CritSection {
    pub fn new() -> Self {
        stop_ints();
        Self
    }
}

impl Drop for CritSection {
    fn drop(&mut self) {
        start_ints();
    }
}

pub(crate) fn start_ints() {
    unsafe {
        match CPUS.get_mut(get_core_id() as usize) {
            Some(core) => {
                if core.crit_stack > 0 {
                    core.crit_stack -= 1;
                }
                if core.crit_stack == 0 {
                    asm!("sti");
                }
            }
            None => asm!("sti"),
        }
    };
}

pub(crate) fn stop_ints() {
    unsafe {
        asm!("cli");
        match CPUS.get_mut(get_core_id() as usize) {
            Some(core) => core.crit_stack += 1,
            None => {}
        }
    };
}

static mut CPUS: Vec<Processor> = vec![];
