
use memmap::MmapMut;

#[derive(Debug)]
pub struct Memory {
  physical: MmapMut
}

impl Memory {
  pub fn new(physical_size: usize) -> Self {
    let physical = MmapMut::map_anon(physical_size).expect("Could not create memory map");
    Self { physical }
  }

  pub fn physical(&mut self) -> &mut MmapMut { &mut self.physical } 
}