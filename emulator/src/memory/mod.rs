pub use region::{Permissions, Region};

mod region;

#[derive(Debug)]
pub enum MemoryError {
    OutOfRegion(u64),
    NotWritable,
    NotExecutable,
}

pub type MemoryResult<T> = Result<T, MemoryError>;

#[derive(Debug)]
pub struct Memory {
    regions: Vec<Region>,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            regions: Vec::new(),
        }
    }

    pub fn add_region(&mut self, region: Region) {
        self.regions.push(region);
    }

    fn find_region_mut(&mut self, address: u64) -> MemoryResult<&mut Region> {
        for region in self.regions.iter_mut().rev() {
            if region.address_range().contains(&address) {
                return Ok(region);
            }
        }

        Err(MemoryError::OutOfRegion(address))
    }

    fn find_region(&self, address: u64) -> MemoryResult<&Region> {
        for region in self.regions.iter().rev() {
            if region.address_range().contains(&address) {
                return Ok(region);
            }
        }

        Err(MemoryError::OutOfRegion(address))
    }

    pub fn slice(&self, base: u64) -> MemoryResult<&[u8]> {
        let region = self.find_region(base)?;
        let offset = base - region.base();

        region.slice(offset)
    }

    pub fn ensure_executable(&self, address: u64) -> MemoryResult<()> {
        let region = self.find_region(address)?;
        region.ensure_executable()
    }

    pub fn store_double_word(&mut self, address: u64, value: u64) -> MemoryResult<()> {
        log::trace!(
            "Writing double word {:#016x} ({}) to memory addresses {:#016x}-{:#016x}",
            value,
            value,
            address,
            address + 7
        );

        let region = self.find_region_mut(address)?;
        let offset = address - region.base();

        region.write(offset + 7, (value >> 56) as u8)?;
        region.write(offset + 6, (value >> 48) as u8)?;
        region.write(offset + 5, (value >> 40) as u8)?;
        region.write(offset + 4, (value >> 32) as u8)?;
        region.write(offset + 3, (value >> 24) as u8)?;
        region.write(offset + 2, (value >> 16) as u8)?;
        region.write(offset + 1, (value >> 8) as u8)?;
        region.write(offset + 0, (value >> 0) as u8)?;

        Ok(())
    }

    pub fn store_word(&mut self, address: u64, value: u64) -> MemoryResult<()> {
        log::trace!(
            "Writing word {:#08x} ({}) to memory address {:#016x}",
            value,
            value,
            address
        );

        let region = self.find_region_mut(address)?;
        let offset = address - region.base();
        region.write(offset + 3, (value >> 24) as u8)?;
        region.write(offset + 2, (value >> 16) as u8)?;
        region.write(offset + 1, (value >> 8) as u8)?;
        region.write(offset + 0, (value >> 0) as u8)?;

        Ok(())
    }

    pub fn store_halfword(&mut self, address: u64, value: u64) -> MemoryResult<()> {
        log::trace!(
            "Writing half-word {:#04x} ({}) to memory address {:#016x}",
            value,
            value,
            address
        );

        let region = self.find_region_mut(address)?;
        let offset = address - region.base();
        region.write(offset + 1, (value >> 8) as u8)?;
        region.write(offset + 0, (value >> 0) as u8)?;

        Ok(())
    }

    pub fn store_byte(&mut self, address: u64, value: u64) -> MemoryResult<()> {
        log::trace!(
            "Writing byte {:#02x} ({}) to memory address {:#016x}",
            value,
            value,
            address
        );
        let region = self.find_region_mut(address)?;
        let offset = address - region.base();
        region.write(offset, value as u8)?;

        Ok(())
    }

    pub fn load_double_word(&self, address: u64) -> MemoryResult<u64> {
        let region = self.find_region(address)?;
        let offset = address - region.base();
        let value = (region.read(offset + 7)? as u64) << 56
            | (region.read(offset + 6)? as u64) << 48
            | (region.read(offset + 5)? as u64) << 40
            | (region.read(offset + 4)? as u64) << 32
            | (region.read(offset + 3)? as u64) << 24
            | (region.read(offset + 2)? as u64) << 16
            | (region.read(offset + 1)? as u64) << 8
            | (region.read(offset + 0)? as u64) << 0;

        log::trace!(
            "Loaded {:#016x} ({}) from memory address {:#016x}",
            value,
            value,
            address
        );

        Ok(value)
    }

    pub fn load_word(&self, address: u64) -> MemoryResult<u64> {
        let region = self.find_region(address)?;
        let offset = address - region.base();
        let value = (region.read(offset + 3)? as u32) << 24
            | (region.read(offset + 2)? as u32) << 16
            | (region.read(offset + 1)? as u32) << 8
            | (region.read(offset + 0)? as u32) << 0;

        log::trace!(
            "Loaded word {:#08x} ({}) from memory address {:#016x}",
            value,
            value,
            address
        );

        Ok(value as i32 as u64)
    }

    pub fn load_word_unsigned(&self, address: u64) -> MemoryResult<u64> {
        let region = self.find_region(address)?;
        let offset = address - region.base();

        let value = (region.read(offset + 3)? as u32) << 24
            | (region.read(offset + 2)? as u32) << 16
            | (region.read(offset + 1)? as u32) << 8
            | (region.read(offset + 0)? as u32) << 0;

        log::trace!(
            "Loaded word {:#08x} ({}) from memory address {:#016x}",
            value,
            value,
            address
        );

        Ok(value as u64)
    }

    pub fn load_halfword(&self, address: u64) -> MemoryResult<u64> {
        let region = self.find_region(address)?;
        let offset = address - region.base();
        let value = (region.read(offset + 1)? as u16) << 8 | (region.read(offset + 0)? as u16) << 0;

        let value = value as i16 as u64;

        log::trace!(
            "Loaded half-word {:#04x} ({}) from memory address {:#016x}",
            value,
            value,
            address
        );

        Ok(value)
    }

    pub fn load_halfword_unsigned(&self, address: u64) -> MemoryResult<u64> {
        let region = self.find_region(address)?;
        let offset = address - region.base();
        let value = (region.read(offset + 1)? as u16) << 8 | (region.read(offset + 0)? as u16) << 0;

        let value = value as u64;

        log::trace!(
            "Loaded unsigned half-word {:#04x} ({}) from memory address {:#016x}",
            value,
            value,
            address
        );

        Ok(value)
    }

    pub fn load_byte(&self, address: u64) -> MemoryResult<u64> {
        let region = self.find_region(address)?;
        let offset = address - region.base();

        let value = region.read(offset)?;
        let value = value as i8 as u64;

        log::trace!(
            "Loaded unsigned byte {:#02x} ({}) from memory address {:#016x}",
            value,
            value,
            address
        );

        Ok(value)
    }

    pub fn load_byte_unsigned(&self, address: u64) -> MemoryResult<u64> {
        let region = self.find_region(address)?;
        let offset = address - region.base();
        let value = region.read(offset)?;
        let value = value as u64;

        log::trace!(
            "Loaded unsigned byte {:#02x} ({}) from memory address {:#016x}",
            value,
            value,
            address
        );

        Ok(value)
    }
}
