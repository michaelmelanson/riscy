use memmap::{Mmap, MmapMut};
use std::ops::Range;
use super::{MemoryError, MemoryResult};

#[derive(Debug)]
pub struct Region {
  backing: RegionBacking,
  base: u64,
  permissions: Permissions
}

impl Region {
  pub fn rom(base: u64, backing: Mmap, permissions: Permissions) -> Region {
    Region {
      backing: RegionBacking::Rom(RomBacking::new(backing)),
      base,
      permissions
    }
  }

  pub fn readwrite_memory(base: u64, size: u64) -> Region {
    Region {
      backing: RegionBacking::init_memory(size),
      base,
      permissions: Permissions::readwrite()
    }
  }

  pub fn make_executable(self) -> Region {
    self.change_permissions(Permissions::executable())
  }

  pub fn change_permissions(self, permissions: Permissions) -> Region {
    Region {
      backing: self.backing,
      base: self.base,
      permissions
    } 
  }

  pub fn base(&self) -> u64 { self.base }
  
  pub fn address_range(&self) -> Range<u64> {
    self.base..(self.base + self.backing.size())
  }

  pub fn slice(&self, base: u64) -> MemoryResult<&[u8]> {
    match self.backing {
      RegionBacking::Rom(ref backing) => Ok(&backing.memory[base as usize..]),
      RegionBacking::Memory(ref backing) => Ok(&backing.memory[base as usize..]),
    }
  }

  pub fn read(&self, offset: u64) -> MemoryResult<u8> {
    let value = match self.backing {
      RegionBacking::Rom(ref backing)       => backing.memory[offset as usize],
      RegionBacking::Memory(ref backing) => backing.memory[offset as usize]
    };

    Ok(value)
  }

  pub fn write(&mut self, offset: u64, value: u8) -> MemoryResult<()> {
    self.ensure_writable()?;

    match self.backing {
      RegionBacking::Rom(_) => panic!("you can't write to ROM; why the hell is it even writable?"),
      RegionBacking::Memory(ref mut backing) => {
        backing.memory[offset as usize] = value;
        Ok(())
      }
    }
  }

  pub fn ensure_writable(&self) -> MemoryResult<()> {
    if !self.permissions.is_writable() {
      return Err(MemoryError::NotWritable);
    }

    Ok(())
  }

  pub fn ensure_executable(&self) -> MemoryResult<()> {
    if !self.permissions.is_executable() {
      return Err(MemoryError::NotExecutable);
    }

    Ok(())
  }
}

#[derive(Debug)]
pub struct Permissions {
  writable: bool,
  executable: bool
}

impl Permissions {
  pub fn custom(writable: bool, executable: bool) -> Permissions {
    Permissions { writable, executable }
  }

  pub fn readonly() -> Permissions { 
    Permissions { writable: false, executable: false }
  }

  pub fn readwrite() -> Permissions { 
    Permissions { writable: true, executable: false }
  }

  pub fn executable() -> Permissions { 
    Permissions { writable: false, executable: true }
  }

  pub fn is_writable(&self) -> bool { self.writable }
  pub fn is_executable(&self) -> bool { self.executable }
}

#[derive(Debug)]
pub enum RegionBacking {
  Rom(RomBacking),
  Memory(MemoryBacking)
}

impl RegionBacking {
  pub fn init_memory(size: u64) -> RegionBacking {
    RegionBacking::Memory(
      MemoryBacking::new(size)
    )
  }

  pub fn size(&self) -> u64 {
    match self {
      RegionBacking::Memory(backing) => backing.size(),
      RegionBacking::Rom(backing) => backing.size()
    }
  }
}

#[derive(Debug)]
pub struct RomBacking {
  memory: Mmap
}

impl RomBacking {
  pub fn new(memory: Mmap) -> Self {
    RomBacking { memory }
  }

  pub fn size(&self) -> u64 {
    self.memory.len() as u64
  }
}

#[derive(Debug)]
pub struct MemoryBacking {
  memory: MmapMut
}

impl MemoryBacking {
  pub fn new(size: u64) -> Self {
    let memory = MmapMut::map_anon(size as usize).expect("Could not create memory map");
    Self { memory }
  }

  pub fn size(&self) -> u64 {
    self.memory.len() as u64
  }
}
