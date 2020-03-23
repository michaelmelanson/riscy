
use clap::{App, Arg};
use memmap::MmapMut;
use corona_emulator::machine::{RiscvMachineStepResult, RiscvMachine};
use corona_emulator::syscall::SystemCall;
use corona_riscv::isa::{Register};

fn main() {
  let matches = App::new("corona-emulator")
    .version("1.0")
    .author("Michael Melanson <michael@michaelmelanson.net>")
    .about("RISC-V emulator")

    .arg(Arg::with_name("FILE")
      .help("A RISC-V binary to run")
      .required(true)
      .takes_value(true)
      .index(1))

    .arg(Arg::with_name("MEMORY")
      .help("Size of memory (in megabytes)")
      .default_value("128"))
    
    .arg(Arg::with_name("LOG_LEVEL")
      .short("d")
      .long("log-level")
      .help("Sets the logging level")
      .possible_values(&["trace", "debug", "info", "warn", "error"])
      .default_value("info"))

    .get_matches();

  std::env::set_var("RUST_LOG", matches.value_of("LOG_LEVEL").expect("LOG_LEVEL"));
  pretty_env_logger::init();

  log::info!("Starting up!");

  let memory_size_mb = matches.value_of("MEMORY")
    .expect("No value for MEMORY");
  let memory_size_mb = memory_size_mb.parse::<usize>().expect("MEMORY must be a positive number");
  let memory_size = memory_size_mb*1024*1024;

  let mut memory = MmapMut::map_anon(memory_size).expect("Could not create memory map");

  let file_path = matches.value_of("FILE").expect("You must give a file to run");
  let file = std::fs::read(file_path).expect("Could not read file");
  let binary = goblin::elf::Elf::parse(&file).expect("Could not parse file");
  
  for ph in binary.program_headers {
    let header_bytes = &file[ph.file_range()];

    for (offset, byte) in header_bytes.iter().enumerate() {
      let address = (ph.p_vaddr as usize) + (offset as usize);
      memory[address] = *byte;
    }
  }

  let mut machine = RiscvMachine::new(memory, binary.header.e_entry);
  machine.state().registers.set(Register::StackPointer, 0x1000);
  while !machine.halted() {
    match machine.step() {
      RiscvMachineStepResult::ExecutedInstruction(_instruction) => {},

      RiscvMachineStepResult::Trap => {
        log::error!("It's a trap!")
      },

      RiscvMachineStepResult::SystemCall => {
        let syscall = SystemCall::from_registers(&machine.state().registers);
        match syscall {
          SystemCall::Break { program_break } => {
            let prior_program_break = machine.state().program_break;
            let is_valid = program_break >= prior_program_break
                              && program_break < machine.state().registers.get(Register::StackPointer)
                              && program_break % 4 == 0;

            if is_valid {
              log::debug!("Updating program break from {:#08x} to {:#08x}", prior_program_break, program_break);
              machine.state().program_break = program_break;
            } else {
              log::debug!("Returning program break of {:#08x}", prior_program_break);
              machine.state().registers.set(Register::A0, prior_program_break);
            }
          },

          SystemCall::Exit { return_code } => {
            log::info!("Exited with code {}", return_code);
            machine.halt()
          },

          SystemCall::Unknown(id) => panic!("Unknown system call: {:?}", id)
        }
      },
    };
  }
}
