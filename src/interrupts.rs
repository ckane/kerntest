use alloc::vec::Vec;
use core::arch::asm;
use core::fmt::write;
use snafu::Snafu;
use log::info;

/// Value to load into GDTR. Implemented as a 128-bit value, but only
/// the lower 80 bits are used (16b length + 64b base). Implements some
/// helper methods for constructing the register value.
#[derive(Copy, Clone)]
pub(crate) struct Gdtr(u128);

/// Entry in the Global Descriptor Table, implemented as a 128-bit value.
/// Even though selector descriptor entries can still be 64 bit, system
/// selector entries are 128 bit. To retain consistency, we just pretend the
/// segment selectors take up 128 bits too, and leave the ones empty that
/// are indexed by values ending in 8.
#[derive(Copy, Clone)]
pub(crate) struct GlobalDescriptorEntry(u128);

/// Value to load into IDTR. Implemented as a 128-bit value, but only
/// the lower 80 bits are used (16b length + 64b base). Implements some
/// helper methods for constructing the register value.
#[derive(Copy, Clone)]
pub(crate) struct Idtr(u128);

/// An interrupt descriptor in the IDT
#[derive(Copy, Clone)]
pub(crate) struct InterruptDescriptorEntry(u128);

/// Encapsulates an Interrupt Stack Table (IST / TSS) as well as manages the stacks
/// pointed at by it
#[derive(Clone)]
pub(crate) struct InterruptStackTable {
    /// The 32 bit entries comprising the IST, including its IOPB
    entries: Vec<u32>,

    /// The stacks for the 7 IST entries
    ists: Vec<Vec<u128>>,

    /// The stacks for the 3 RSP entries for PL changes
    rsps: Vec<Vec<u128>>,
}

/// Stack frame for interrupt routines. Must be declared repr(C) so that its contents
/// can be mapped onto a C-like data layout
#[derive(Clone)]
#[repr(C)]
pub struct InterruptStack {
    rip: u64,
    cs: u16,
    rfl: u64,
    rsp: u64,
    ss: u16
}

impl core::fmt::Debug for InterruptStack {
    /// Write out the contents of an InterruptStack
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::result::Result<(), core::fmt::Error> {
        write!(fmt, "{{ rip: {:#018x}, cs: {:#06x}, rflags: {:#018x}, rsp: {:#018x}, ss: {:#06x} }}",
            self.rip, self.cs, self.rfl, self.rsp, self.ss)
    }
}

#[derive(Debug, Snafu)]
pub(crate) enum Error {
    /// Data or Type conversion error
    ConversionError,
}

pub(crate) type Result<T> = core::result::Result<T, Error>;

impl<'a> From<&'a InterruptStackTable> for &'a [u32] {
    /// Implement conversion between IST and a u32 slice reference
    fn from(ist: &InterruptStackTable) -> &[u32] {
        &ist.entries
    }
}

/// Default offset of IOPB from beginning of IST
const IOPB_OFFSET: u16 = 0x68;

/// Hard-coded stack sizes for ISTs and RSPs in the IST (1MB each)
const IST_STACK_SIZE: usize = 1048576;

impl Default for InterruptStackTable {
    fn default() -> Self {
        // (1 reserved + 3 x (2 per RSP) + 2 reserved +
        // 7 x (2 per IST) + 2 reserved + 1 IOPB offset) = 0x68
        // + 2048 for IOPB
        let mut entries_vec = vec![0; IOPB_OFFSET as usize + 2048];

        // Describe that the IOPB is 0x68 (104) bytes after the beginning
        // of this IST
        entries_vec[0x64] = (IOPB_OFFSET as u32) << 16;

        let mut new_self = Self {
            entries: entries_vec,
            ists: vec![vec![0; IST_STACK_SIZE / 16]; 7],
            rsps: vec![vec![0; IST_STACK_SIZE / 16]; 3],
        };

        // Iterate across each of the 3 privilege-change stacks, and write their address into the
        // IST
        for r in [
            ISTPrivilegeStack::RSP0,
            ISTPrivilegeStack::RSP1,
            ISTPrivilegeStack::RSP2,
        ] {
            let mut addr = new_self.rsps[usize::from(r)].as_mut_ptr() as usize;
            addr += new_self.rsps[usize::from(r)].len() * 16 - 1;
            addr &= !0xf;
            new_self.set_rsp(r, addr);
        }

        // Iterate across each of the 7 IST stacks, and write their address into the
        // IST
        for i in [
            ISTStacks::IST1,
            ISTStacks::IST2,
            ISTStacks::IST3,
            ISTStacks::IST4,
            ISTStacks::IST5,
            ISTStacks::IST6,
            ISTStacks::IST7,
        ] {
            let mut addr = new_self.ists[usize::from(i) - 1].as_mut_ptr() as usize;
            addr += new_self.ists[usize::from(i) - 1].len() * 16 - 1;
            addr &= !0xf;
            new_self.set_ist(i, addr);
        }

        new_self.enable_all_iop();

        new_self
    }
}

#[derive(Copy, Clone, Debug)]
enum ISTPrivilegeStack {
    RSP0,
    RSP1,
    RSP2,
}

#[derive(Copy, Clone, Debug)]
enum ISTStacks {
    IST1,
    IST2,
    IST3,
    IST4,
    IST5,
    IST6,
    IST7,
}

impl TryFrom<usize> for ISTPrivilegeStack {
    type Error = Error;
    fn try_from(v: usize) -> Result<Self> {
        match v {
            0 => Ok(ISTPrivilegeStack::RSP0),
            1 => Ok(ISTPrivilegeStack::RSP1),
            2 => Ok(ISTPrivilegeStack::RSP2),
            _ => Err(Error::ConversionError),
        }
    }
}

impl TryFrom<usize> for ISTStacks {
    type Error = Error;
    fn try_from(v: usize) -> Result<Self> {
        match v {
            1 => Ok(ISTStacks::IST1),
            2 => Ok(ISTStacks::IST2),
            3 => Ok(ISTStacks::IST3),
            4 => Ok(ISTStacks::IST4),
            5 => Ok(ISTStacks::IST5),
            6 => Ok(ISTStacks::IST6),
            7 => Ok(ISTStacks::IST7),
            _ => Err(Error::ConversionError),
        }
    }
}

impl From<ISTPrivilegeStack> for usize {
    fn from(stack: ISTPrivilegeStack) -> usize {
        match stack {
            ISTPrivilegeStack::RSP0 => 0,
            ISTPrivilegeStack::RSP1 => 1,
            ISTPrivilegeStack::RSP2 => 2,
        }
    }
}

impl From<ISTStacks> for usize {
    fn from(stack: ISTStacks) -> usize {
        match stack {
            ISTStacks::IST1 => 1,
            ISTStacks::IST2 => 2,
            ISTStacks::IST3 => 3,
            ISTStacks::IST4 => 4,
            ISTStacks::IST5 => 5,
            ISTStacks::IST6 => 6,
            ISTStacks::IST7 => 7,
        }
    }
}

impl InterruptStackTable {
    pub fn get_ist_len_bytes(&self) -> usize {
        self.entries.len() * 4
    }
    pub fn get_ist_ptr(&mut self) -> Option<*mut u32> {
        match self.entries.len() {
            0 => None,
            _ => Some(self.entries.as_mut_ptr()),
        }
    }

    pub fn set_rsp(&mut self, dpl: ISTPrivilegeStack, addr: usize) {
        let rsp = dpl as usize;
        self.entries[rsp * 2 + 1] = (addr & 0xffffffff) as u32;
        self.entries[rsp * 2 + 2] = ((addr >> 32) & 0xffffffff) as u32;
    }

    pub fn get_rsp(&self, dpl: ISTPrivilegeStack) -> usize {
        let rsp = dpl as usize;
        (self.entries[rsp * 2 + 1] as usize) | ((self.entries[rsp * 2 + 2] as usize) << 32)
    }

    pub fn set_ist(&mut self, index: ISTStacks, addr: usize) {
        let ist = index as usize;
        self.entries[ist * 2 + 9] = (addr & 0xffffffff) as u32;
        self.entries[ist * 2 + 10] = ((addr >> 32) & 0xffffffff) as u32;
    }

    pub fn get_ist(&self, index: ISTStacks) -> usize {
        let ist = index as usize;
        (self.entries[ist * 2 + 9] as usize) | ((self.entries[ist * 2 + 10] as usize) << 32)
    }

    /// Returns Some(()) if port is permitted in IOPB, and None if not permitted
    pub fn get_iop(&self, port: u16) -> Option<()> {
        ((self.entries[IOPB_OFFSET as usize + (port as usize) / 32] >> (port as usize % 8)) & 0x1 == 0).then(|| ())
    }

    /// Set whether a port in the IOPB is enabled or disabled
    pub fn set_iop(&mut self, port: u16, enable: bool) {
        match enable {
            true => self.entries[IOPB_OFFSET as usize + (port as usize) / 32] &= !1 << (port as usize % 8),
            false => self.entries[IOPB_OFFSET as usize + (port as usize) / 32] |= 1 << (port as usize % 8),
        }
    }

    pub fn enable_all_iop(&mut self) {
        for block in (IOPB_OFFSET as usize)..self.entries.len() {
            self.entries[block] = 0;
        }
    }

    pub fn disable_all_iop(&mut self) {
        for block in (IOPB_OFFSET as usize)..self.entries.len() {
            self.entries[block] = 0xffffffff;
        }
    }
}

impl core::fmt::Debug for InterruptStackTable {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::result::Result<(), core::fmt::Error> {
        write!(f, "{{ ")?;
        for i in [
            ISTStacks::IST1,
            ISTStacks::IST2,
            ISTStacks::IST3,
            ISTStacks::IST4,
            ISTStacks::IST5,
            ISTStacks::IST6,
            ISTStacks::IST7,
        ] {
            write!(f, "IST{}: {:#018x}, ", usize::from(i), self.get_ist(i))?;
        };

        for r in [
            ISTPrivilegeStack::RSP0,
            ISTPrivilegeStack::RSP1,
            ISTPrivilegeStack::RSP2,
        ] {
            write!(f, "RSP{}: {:#018x}, ", usize::from(r), self.get_rsp(r))?;
        };
        write!(f, " }}")?;

        Ok(())
    }
}

impl From<&Idtr> for u128 {
    fn from(i: &Idtr) -> Self {
        i.0
    }
}

impl From<&Gdtr> for u128 {
    fn from(g: &Gdtr) -> Self {
        g.0
    }
}

impl From<&GlobalDescriptorEntry> for u128 {
    fn from(g: &GlobalDescriptorEntry) -> Self {
        g.0
    }
}

impl From<&InterruptDescriptorEntry> for u128 {
    fn from(i: &InterruptDescriptorEntry) -> Self {
        i.0
    }
}

impl Default for InterruptDescriptorEntry {
    fn default() -> Self {
        Self(0)
    }
}

impl Default for GlobalDescriptorEntry {
    fn default() -> Self {
        Self(0)
    }
}

impl InterruptDescriptorEntry {
    pub fn new_trap(func: extern "x86-interrupt" fn(InterruptStack), seg: u16, pl: u8, ist: u8) -> Self {
        let offset = func as usize;
        Self(
            (offset as u128 & 0xffff)
                | (seg as u128) << 16
                | (ist as u128) << 32
                | (0xf << 40)
                | (pl as u128 | 4) << 45
                | (1 as u128) << 47 // +P
                | (offset as u128 & 0xffffffffffff0000) << 32,
        )
    }

    pub fn new_int(offset: usize, seg: u16, pl: u8, ist: u8) -> Self {
        Self(
            (offset as u128 & 0xffff)
                | (seg as u128) << 16
                | (ist as u128) << 32
                | (0xe << 40)
                | (pl as u128 | 4) << 45
                | (1 as u128) << 47 // +P
                | (offset as u128 & 0xffffffffffff0000) << 32,
        )
    }
}

impl GlobalDescriptorEntry {
    pub fn new_codeseg(dpl: u8) -> Self {
        Self(
            0xaf    << 48 | // Flags: +G +L, Limit: 0xf
            (dpl as u128 & 0x3) << 45 |
            //0x9f << 40 |   // Access: +P, +S, +E, +DC, +R, +A
            0x9a << 40 |   // Access: +P, +S, +E, +DC, +R, +A
            0xffff, // Limit: 0xffff
        )
    }
    pub fn new_dataseg(dpl: u8) -> Self {
        Self(
            0xcf    << 48 | // Flags: +G +DB, Limit: 0xf
            (dpl as u128 & 0x3) << 45 |
            //0x93 << 40  | // Access: +P, +S, +W, +A
            0x92 << 40  | // Access: +P, +S, +W, +A
            0xffff, // Limit: 0xffff
        )
    }
    pub fn new_istseg(addr: usize, len: usize) -> Self {
        Self(
            0x89 << 40  | // Access: +P, +Avl (64b IST)
            ((addr as u128 & !0xffffff) << 32) | // Base[32..64]
            ((len as u128 - 1) & 0xff0000) | // Limit[4] (len - 1)
            ((addr as u128 & 0xffffff) << 16) | // Base[0..5]
            ((len as u128 - 1) & 0xffff), // Limit[0..4] (len - 1)
        )
    }
}

impl Gdtr {
    pub fn new(gdt: &[GlobalDescriptorEntry]) -> Self {
        Self(
            (gdt.len() as u128 * (core::mem::size_of::<GlobalDescriptorEntry>() as u128))
                | (gdt.as_ptr() as u128) << 16,
        )
    }

    pub fn lgdt(&self, codeseg: u16, dataseg: u16) {
        unsafe {
            asm!(
                "lgdt [{}]",
                "mov rax, {}",
                "mov ds, ax",
                "mov ss, ax",
                "mov es, ax",
                "mov fs, ax",
                "mov gs, ax",
                "mov rax, {}",
                "push rax",
                "lea rax, [22f + rip]",
                "push rax",
                "retfq",
                "22:",
                in(reg) self,
                in(reg) dataseg,
                in(reg) codeseg,
                options(readonly, nostack, preserves_flags)
            );
        };
    }
}

impl Idtr {
    pub fn new(idt: &[InterruptDescriptorEntry]) -> Self {
        Self(
            (idt.len() as u128 * (core::mem::size_of::<InterruptDescriptorEntry>() as u128))
                | (idt.as_ptr() as u128) << 16,
        )
    }

    pub fn lidt(&self) {
        unsafe {
            asm!(
                "lidt [{}]",
                in(reg) self
            );
        };
    }
}
