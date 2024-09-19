pub struct Gdtr(u128);
pub struct GlobalDescriptorEntry(u128);

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

impl GlobalDescriptorEntry {
    pub fn new_null() -> Self {
        Self(0)
    }
    pub fn new_codeseg(dpl: u8) -> Self {
        Self(
            0          |
            0xaf    << 48 | // Flags: +G +L, Limit: 0xf
            (dpl as u128 & 0x3) << 45 |
            //0x9f << 40 |   // Access: +P, +S, +E, +DC, +R, +A
            0x9a << 40 |   // Access: +P, +S, +E, +DC, +R, +A
            0xffff         // Limit: 0xffff
        )
    }
    pub fn new_dataseg(dpl: u8) -> Self {
        Self(
            0          |
            0xcf    << 48 | // Flags: +G +DB, Limit: 0xf
            (dpl as u128 & 0x3) << 45 |
            //0x93 << 40  | // Access: +P, +S, +W, +A
            0x92 << 40  | // Access: +P, +S, +W, +A
            0xffff         // Limit: 0xffff
        )
    }
}

impl Gdtr {
    pub fn new(gdt: &[GlobalDescriptorEntry]) -> Self {
        Self(
            (gdt.len() as u128 * (core::mem::size_of::<GlobalDescriptorEntry>() as u128)) |
            (gdt.as_ptr() as u128) << 16
        )
    }
}
