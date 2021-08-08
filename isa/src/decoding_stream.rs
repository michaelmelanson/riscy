use byteorder::{LittleEndian, ReadBytesExt};
use std::io::Cursor;

use crate::Instruction;

pub struct DecodingStream<'a> {
    cursor: Cursor<&'a [u8]>,
}

impl<'a> DecodingStream<'a> {
    pub fn new(bytes: &'a [u8]) -> DecodingStream {
        DecodingStream {
            cursor: Cursor::new(bytes),
        }
    }
}

impl<'a> Iterator for DecodingStream<'a> {
    type Item = Instruction;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        let base: u16 = self.cursor.read_u16::<LittleEndian>().ok()?;

        if base == 0 {
            return None;
        }

        let ones = base.trailing_ones();

        match ones {
            0..=1 => Some(Instruction::from_16bits(base)),

            2..=4 => {
                let second: u32 = self.cursor.read_u16::<LittleEndian>().unwrap() as u32;
                let encoded: u32 = (second << 16) | (base as u32);

                Some(Instruction::from_32bits(encoded))
            }

            5 => {
                let parcels = [
                    base,
                    self.cursor.read_u16::<LittleEndian>().unwrap(),
                    self.cursor.read_u16::<LittleEndian>().unwrap(),
                ];

                unimplemented!("48-bit instruction: {:?}", parcels);
            }

            6 => {
                let parcels = [
                    base,
                    self.cursor.read_u16::<LittleEndian>().unwrap(),
                    self.cursor.read_u16::<LittleEndian>().unwrap(),
                    self.cursor.read_u16::<LittleEndian>().unwrap(),
                ];

                unimplemented!("64-bit instruction: {:?}", parcels);
            }

            7..=11 => unimplemented!("Large instruction ({} trailing ones): {:#018b}", ones, base),
            _ => unimplemented!(
                "Invalid instruction ({} trailing ones): {:#018b}",
                ones,
                base
            ),
        }
    }
}
