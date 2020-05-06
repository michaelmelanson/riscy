use clap::{Arg, App};
use riscy_emulator::{
  memory::Memory, 
  subsystem::{Posix}, 
  machine::{RiscvMachine, RiscvMachineError, RiscvMachineStepAction},
};
use riscy_isa::{Register};
use std::{
  io::{Write, Read},
  process::{Stdio, Command},
  thread::JoinHandle, 
};

use crossbeam_channel::{
  bounded,
  Receiver, Sender
};

pub fn main() -> Result<(), RiscvMachineError> {
  let matches = App::new("riscy-cosimulator")
    .about("Cosimulates a RISC-V binary in RISCY and Spike to check for discrepencies")

    .arg(Arg::with_name("FILE")
      .help("A RISC-V binary to disassemble")
      .required(true)
      .takes_value(true)
      .index(1))

    .arg(Arg::with_name("MEMORY")
      .short("m")
      .long("memory")
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

  let memory_size_mb = matches.value_of("MEMORY").expect("No value for MEMORY")
    .parse::<usize>().expect("MEMORY must be a positive number");
  let file_path = matches.value_of("FILE").expect("You must give a file to run");

  let memory_size = memory_size_mb*1024*1024;

  let riscy_sim = RiscySimulator::new(memory_size, file_path);
  let spike_sim = SpikeSimulator::new(file_path);

  let mut cosimulator = Cosimulator::new(riscy_sim, spike_sim);
  cosimulator.run(&[
    // 0x00000000001000 + 8*0,
    // 0x00000000001000 + 8*1,
    // 0x00000000001000 + 8*2,
    // 0x00000000001000 + 8*3,
    // 0x00000000001000 + 8*4,
    // 0x00000000001000 + 8*5,
    // 0x00000000001000 + 8*6,
    // 0x00000000001000 + 8*7
  ]).expect("cosimulation failed");

  Ok(())
}

#[derive(Debug, Eq, PartialEq)]
struct SimulatorState {
  pc: u64,
  registers: Vec<(Register, u64)>,
  memory: Vec<(u64, u64)>,
  halted: bool
}

trait Simulator {
  fn state(&mut self, memory: &[u64]) -> SimulatorState;
  fn step(&mut self);
}

struct RiscySimulator {
  machine: RiscvMachine<Posix>
}

impl RiscySimulator {
  fn new(memory_size: usize, path: &str) -> Self {
    let mut memory = Memory::new(memory_size);
    
    let file = std::fs::read(path).expect("Could not read file");
    let binary = goblin::elf::Elf::parse(&file).expect("Could not parse file");
    
    let entry_virt_address = binary.header.e_entry;

    for ph in binary.program_headers {
      let header_bytes = &file[ph.file_range()];
      
      log::debug!("Loading header {:?} from {:?} into {:#016x}", ph, ph.file_range(), ph.p_vaddr);
      
      for (offset, byte) in header_bytes.iter().enumerate() {
        memory.physical()[(ph.p_paddr as usize) + (offset as usize)] = *byte;
      }
    }

    log::debug!("Entry point is {:#016x}", entry_virt_address);
    
    const XLEN: u32 = 64;
    const RESET_VEC_BASE: usize = 0x1000;
    const RESET_VEC_SIZE: u32 = 8;

    let mut reset_vec = [
      0x297,                                      // auipc  t0,0x0
      0x28593 + (RESET_VEC_SIZE * 4 << 20),       // addi   a1, t0, &dtb
      0xf1402573,                                 // csrr   a0, mhartid
      if XLEN == 32 { 0x0182a283 }                // lw     t0,24(t0)
      else          { 0x0182b283 },               // ld     t0,24(t0)
      0x28067,                                    // jr     t0
      0,
      (entry_virt_address & 0xffffffff) as u32,
      (entry_virt_address >> 32) as u32
    ].to_vec();

    let dtb: &[u8] = include_bytes!("../spike.dtb");
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


    for (offset, x) in reset_vec.iter().enumerate() {
      memory.physical()[RESET_VEC_BASE + (4*offset) + 0] = (x >> 0) as u8;
      memory.physical()[RESET_VEC_BASE + (4*offset) + 1] = (x >> 8) as u8;
      memory.physical()[RESET_VEC_BASE + (4*offset) + 2] = (x >> 16) as u8;
      memory.physical()[RESET_VEC_BASE + (4*offset) + 3] = (x >> 24) as u8;
    }

    let machine= RiscvMachine::<Posix>::new(memory, RESET_VEC_BASE as u64);

    RiscySimulator { machine }
  }

}

impl Simulator for RiscySimulator {
    fn state(&mut self, memory_addresses: &[u64]) -> SimulatorState {
      let state = self.machine.state();

      let pc = state.pc;
      let halted = self.machine.halted();

      let mut registers = Vec::new();

      for index in 0..32 {
        let register = Register::from_u8(index);
        registers.push((register, self.machine.state().registers.get(register)));
      }


      let mut memory = Vec::new();
      for address in memory_addresses {
        let value = self.machine.load_double_word(*address);
        memory.push((*address, value));
      }
        
      SimulatorState { pc, halted, registers, memory }
    }

    fn step(&mut self) {
      match self.machine.step().expect("step failed") {
        RiscvMachineStepAction::ExecutedInstruction { instruction: _ } => {},
        RiscvMachineStepAction::Exit { status_code } => {
          log::info!("Exited with code {}", status_code);
          self.machine.halt()
        }
      }
    }
}


#[derive(Debug)]
enum SpikeSimulatorCommand {
  Send(String),
  Terminate
}

#[derive(Debug)]
enum SpikeSimulatorEvent {
  Output(String),
}

struct SpikeSimulator(Sender<SpikeSimulatorCommand>, Receiver<SpikeSimulatorEvent>, JoinHandle<()>);

impl SpikeSimulator {
  fn new(path: &str) -> Self {
    let mut child = Command::new("/opt/riscv/bin/spike")
      .arg("-d")
      .arg(path)
      .stdin(Stdio::piped())
      .stdout(Stdio::null())
      .stderr(Stdio::piped())
      .spawn()
      .expect("failed to launch spike");

    let (command_sender, command_receiver) = bounded(1);
    let (event_sender, event_receiver) = bounded(1);

    let handle = std::thread::spawn(move || {
      let stdin = child.stdin.as_mut().expect("stdin");
      let mut stderr = child.stderr.expect("stderr").bytes();

      loop {
        let command = command_receiver.recv().expect("receive command");

        match command {
          SpikeSimulatorCommand::Send(s) => writeln!(stdin, "{}", s).expect("write to stdin"),
          SpikeSimulatorCommand::Terminate => return ()
        };

        let mut response = String::new();

        'read: loop {
          if let Some(c) = stderr.next() {
            let c = c.expect("read stderr");

            match c as char {
              '\n' => break 'read,
              c => response.push(c)
            }
          } else {
            return ();
          }
        }

        let (_, response) = response.split_at(2);

        event_sender.send(SpikeSimulatorEvent::Output(response.to_string())).expect("send response");
      }
    });

    SpikeSimulator(command_sender, event_receiver, handle)
  }

  fn send_command(&mut self, command: &str) -> String {
    self.0.send(SpikeSimulatorCommand::Send(command.to_string())).expect("sending command");
    let response = self.1.recv().expect("receiving response");

    match response {
      SpikeSimulatorEvent::Output(output) => output
    }
  }
}

impl Simulator for SpikeSimulator {
    fn state(&mut self, memory_addresses: &[u64]) -> SimulatorState {
      let pc_string = self.send_command("pc 0");
      let (prefix, pc_number) = pc_string.split_at(2);
      if prefix != "0x" {
        panic!("unexpected prefix {:?} on PC response: {:?}", prefix, pc_string);
      }

      let pc = u64::from_str_radix(pc_number, 16).expect("parse pc");

      let halted = false;

      let mut registers = Vec::new();

      for index in 0..32 {
        let register = Register::from_u8(index);
        let value = self.send_command(&format!("reg 0 {}", index));
        let value = u64::from_str_radix(&value[2..], 16).expect(&format!("parsing {:?} for register {:?}", value, register));

        registers.push((register, value));
      }

      let mut memory = Vec::new();
      for address in memory_addresses {
        let value = self.send_command(&format!("mem {:#x}", address));
        let value = u64::from_str_radix(&value[2..], 16).expect(&format!("parsing {:?} for memory address {:?}", value, address));
        memory.push((*address, value));
      }
        
      SimulatorState { pc, halted, registers, memory }
    }

    fn step(&mut self) {
      let response = self.send_command("");
      log::info!("Spike> {}", response);
    }
}

struct Cosimulator<Experiment: Simulator, Control: Simulator> { experiment: Experiment, control: Control }

impl <Experiment: Simulator, Control: Simulator> Cosimulator<Experiment, Control> {

  fn new(experiment: Experiment, control: Control) -> Self {
    Cosimulator { experiment, control }
  }

  fn run(&mut self, important_memory: &[u64]) -> Result<(), ()> {
    loop {
      let experiment_state = self.experiment.state(important_memory);
      let control_state = self.control.state(important_memory);

      if experiment_state != control_state {
        log::error!("Simulators out of sync:\nExperiment (Riscy): {:016X?}\nControl (spike): {:016X?}", experiment_state, control_state);
        return Err(())
      }

      if control_state.halted {
        break;
      }

      self.experiment.step();
      self.control.step();
    }

    Ok(())
  }
}