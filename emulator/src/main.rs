use clap::{App, Arg};
use riscy_emulator::machine::{RiscvMachine, RiscvMachineError, RiscvMachineStepAction};
use riscy_emulator::{subsystem::{Subsystem, Posix}, memory::Memory};
use riscy_isa::{Register};

fn main()  -> Result<(), RiscvMachineError> {
  let matches = App::new("riscv-emulator")
    .version("1.0")
    .author("Michael Melanson <michael@michaelmelanson.net>")
    .about("A RISC-V emulator")

    .arg(Arg::with_name("FILE")
      .help("A RISC-V binary to run")
      .required(true)
      .takes_value(true)
      .index(1))

    .arg(Arg::with_name("MEMORY")
      .short("m")
      .long("memory")
      .help("Size of memory (in megabytes)")
      .default_value("128"))

    
    .arg(Arg::with_name("SUBSYSTEM")
      .short("s")
      .long("subsystem")
      .help("Selects which kind of machine to emulate")
      .possible_values(&["posix"])
      .default_value("posix"))
    
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

  let memory_size_mb = matches.value_of("MEMORY").expect("No value for MEMORY")
    .parse::<usize>().expect("MEMORY must be a positive number");
  let file_path = matches.value_of("FILE").expect("You must give a file to run");
  let subsystem = matches.value_of("SUBSYSTEM").expect("You must give a SUBSYSTEM");

  let memory_size = memory_size_mb*1024*1024;

  let mut memory = Memory::new(memory_size);

  let file = std::fs::read(file_path).expect("Could not read file");
  let binary = goblin::elf::Elf::parse(&file).expect("Could not parse file");
  
  for ph in binary.program_headers {
    let header_bytes = &file[ph.file_range()];

    log::debug!("Loading header {:?} from {:?} into {:#016x}", ph, ph.file_range(), ph.p_vaddr);

    for (offset, byte) in header_bytes.iter().enumerate() {
      let address = (ph.p_vaddr as usize) + (offset as usize);
      memory.physical()[address] = *byte;
    }
  }

  log::debug!("Entry point is {:#016x}", binary.header.e_entry);

  match subsystem {
    "posix" => run_machine::<Posix>(memory, binary.header.e_entry),

    _ => panic!("Unknown subsystem {}", subsystem)
  }
}

fn run_machine<S: Subsystem>(memory: Memory, entry: u64) -> Result<(), RiscvMachineError>{
  let mut machine= RiscvMachine::<S>::new(memory, entry);
  machine.state_mut().registers.set(Register::StackPointer, 0x1000);
  while !machine.halted() {
    match machine.step()? {
      RiscvMachineStepAction::ExecutedInstruction { instruction: _ } => {},
      RiscvMachineStepAction::Exit { status_code } => {
        log::info!("Exited with code {}", status_code);
        machine.halt()
      }
    }
  }
  Ok(())
}