use alloc::vec::Vec;

#[repr(C)]
#[repr(packed(8))]
#[derive(Clone, Copy, Default)]
pub struct ThreadContext {
    pub frame: crate::interrupts::InterruptStack,
    rax: u64,
    rcx: u64,
    rdx: u64,
    rbx: u64,
    rbp: u64,
    rsi: u64,
    rdi: u64,
    r08: u64,
    r09: u64,
    r10: u64,
    r11: u64,
    r12: u64,
    r13: u64,
    r14: u64,
    r15: u64,
    rip: u64,
    cs: u64,
    flags: u64,
    rsp: u64,
    ss: u64,
}

impl core::fmt::Debug for ThreadContext {
    /// Write out the contents of an InterruptStack
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::result::Result<(), core::fmt::Error> {
        write!(fmt, "{{ rax: {:#018x}, rcx: {:#018x}, rdx: {:#018x}, rbx: {:#018x}, rbp: {:#018x}, ",
            self.rax, self.rcx, self.rdx, self.rbx, self.rbp)?;
        write!(fmt, "rsi: {:#018x}, rdi: {:#018x}, r8: {:#018x}, r9: {:#018x}, r10: {:#018x}, ",
            self.rsi, self.rdi, self.r08, self.r09, self.r10)?;
        write!(fmt, "r11: {:#018x}, r12: {:#018x}, r13: {:#018x}, r14: {:#018x}, r15: {:#018x}, ",
            self.r11, self.r12, self.r13, self.r14, self.r15)?;
        write!(fmt, "rip: {:#018x}, cs: {:#018x}, flags: {:#018x}, rsp: {:#018x}, ss: {:#018x} }}",
            self.rip, self.cs, self.flags, self.rsp, self.ss)?;
        Ok(())
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Thread {
    saved_context: ThreadContext,
    id: usize,
    stack: alloc::boxed::Box<Vec<usize>>,
}

impl Thread {
    pub fn new(id: usize, ss: u64, cs: u64, rip: u64) -> Self {
        let mut s = Self {
            id,
            stack: alloc::boxed::Box::new(vec![0; 65536]),
            saved_context: ThreadContext::default(),
        };
        s.saved_context.rdi = id as u64;
        s.saved_context.ss = ss;
        s.saved_context.frame.ss = ss;
        s.saved_context.cs = cs;
        s.saved_context.frame.cs = cs;
        s.saved_context.rip = rip;
        s.saved_context.frame.rip = rip;
        s.saved_context.rsp = unsafe { s.stack.as_ptr().add(65535) } as u64;
        s.saved_context.frame.rsp = s.saved_context.rsp;
        s.saved_context.flags = 0x292;
        s.saved_context.frame.rfl = 0x292;
        s
    }

    pub fn save_context(&mut self, context: ThreadContext) {
        self.saved_context = context;

        // When saving context, always enable IF so interrupts get reset on iretq from context
        // switch
        self.saved_context.flags |= 0x200;
        self.saved_context.frame.rfl |= 0x200;
    }

    pub fn get_context(&self) -> ThreadContext {
        self.saved_context
    }
}
