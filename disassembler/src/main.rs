use clap::{App, Arg};
use riscy_isa::DecodingStream;
use std::ops::Range;

pub fn main() {
    let matches = App::new("riscy-dissambler")
        .about("Disassembles ELF binaries containing RISC-V code")
        .arg(
            Arg::with_name("FILE")
                .help("A RISC-V binary to disassemble")
                .required(true)
                .takes_value(true)
                .index(1),
        )
        .get_matches();

    let file_path = matches.value_of("FILE").expect("FILE not provided");

    pretty_env_logger::init();

    let file = std::fs::read(file_path).expect("Could not read file");
    let binary = goblin::elf::Elf::parse(&file).expect("Could not parse file");

    let mut symbols = binary.syms.to_vec();
    symbols.sort_by_cached_key(|s| s.st_value);

    for window in symbols.windows(2) {
        let symbol = window[0];
        let next = window[1];

        let header: Option<goblin::elf::ProgramHeader> = {
            let mut result = None;

            for header in &binary.program_headers {
                if header.vm_range().contains(&(symbol.st_value as usize)) {
                    result = Some(header.clone());
                }
            }

            result
        };

        if let Some(range) = header.map(|header| {
            let start = (symbol.st_value - header.p_vaddr) as usize;
            let end = (next.st_value - header.p_vaddr) as usize;
            Range::from(start..end)
        }) {
            if symbol.is_function() {
                let name = binary
                    .strtab
                    .get(symbol.st_name)
                    .unwrap_or(Ok("<no name>"))
                    .unwrap_or("<invalid symbol>");

                println!("{:#0x}: {}", symbol.st_value, name);

                let bytes = &file[range];
                let stream = DecodingStream::new(bytes);
                for instruction in stream {
                    println!("    {:X?}", instruction);
                }
            }
        }
    }

    for ph in binary.program_headers {
        println!("Program header: {:?}", ph);

        if !ph.is_executable() {
            continue; // it's probably not code
        }

        let header_bytes = &file[ph.file_range()];
        let stream = DecodingStream::new(header_bytes);

        println!("Header bytes: {:x?}", header_bytes);

        for instruction in stream {
            println!("{:?}", instruction);
        }
    }
}
