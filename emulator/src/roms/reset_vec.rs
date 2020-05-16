
use crate::memory::{Region, Permissions};

pub struct ResetVecRom { entry_address: u64 }
impl ResetVecRom {
  pub fn new(entry_address: u64) -> Self {
    ResetVecRom { entry_address }
  }
}

impl Into<Region> for ResetVecRom {
  fn into(self) -> Region {
    const XLEN: u32 = 64;
    const RESET_VEC_BASE: u64 = 0x1000;
    const RESET_VEC_SIZE: u32 = 8;
  
    let mut reset_vec = [
      0x297,                                      // auipc  t0,0x0
      0x28593 + (RESET_VEC_SIZE * 4 << 20),       // addi   a1, t0, &dtb
      0xf1402573,                                 // csrr   a0, mhartid
      if XLEN == 32 { 0x0182a283 }                // lw     t0,24(t0)
      else          { 0x0182b283 },               // ld     t0,24(t0)
      0x28067,                                    // jr     t0
      0,
      (self.entry_address & 0xffffffff) as u32,
      (self.entry_address >> 32) as u32
    ].to_vec();
  
    let dtb: &[u8] = include_bytes!("./spike.dtb");
    for offset in (0..dtb.len()-7).step_by(8) {
      let low = 
        (dtb[offset + 0] as u32) << 0 |
        (dtb[offset + 1] as u32) << 8 |
        (dtb[offset + 2] as u32) << 16 |
        (dtb[offset + 3] as u32) << 24;
  
      let high = 
        (dtb[offset + 4] as u32) << 0 |
        (dtb[offset + 5] as u32) << 8 |
        (dtb[offset + 6] as u32) << 16 |
        (dtb[offset + 7] as u32) << 24;
  
        reset_vec.push(low);
        reset_vec.push(high);
    }
  
    let rom_size = reset_vec.len() * 4;
    let mut mmap = memmap::MmapMut::map_anon(rom_size).expect("mmap");
  
    for (offset, x) in reset_vec.iter().enumerate() {
      mmap[(4*offset) + 0] = (x >> 0) as u8;
      mmap[(4*offset) + 1] = (x >> 8) as u8;
      mmap[(4*offset) + 2] = (x >> 16) as u8;
      mmap[(4*offset) + 3] = (x >> 24) as u8;
    }

    let mmap = mmap.make_read_only().expect("convert mmap to read-only");

    Region::rom(RESET_VEC_BASE, mmap, Permissions::executable())
  }
}

