fn main() {
  let bytes = include_bytes!("../../tests/add.m");

  match goblin::elf::Elf::parse(bytes) {
    Ok(binary) => {
      println!("Binary: {:?}", binary);
      let entry = binary.entry;
      println!("Entry: {:?}", entry);
      for ph in binary.program_headers {
        println!("Program header: {:?}", ph);
        
        let header_bytes = &bytes[ph.file_range()];
        println!("Bytes: {:?}", header_bytes);
      }

    },
    Err(_) => ()
  }
}