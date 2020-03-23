use memmap::MmapMut;
use corona_emulator::machine::{RiscvMachineStepResult, RiscvMachine};
use corona_riscv::isa::{Register};

pub fn run_test_suite(file: &[u8]) {
  let binary = goblin::elf::Elf::parse(file).expect("Could not parse file");
  
  let mut memory = MmapMut::map_anon(1024*1024*1024*4).expect("Could not create memory map");

  for ph in binary.program_headers {
    // println!("Loading header: {:?}", ph);
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
      RiscvMachineStepResult::ExecutedInstruction(_instruction) => {
        // println!("Instruction: {:?}", instruction)
      },

      RiscvMachineStepResult::Trap => {
        panic!("It's a trap!");
      },

      RiscvMachineStepResult::SystemCall => {
        let state = machine.state();
        let syscall = state.registers.get(Register::A7);

        match syscall {
          93 => {
            let testnum = state.registers.get(Register::GlobalPointer);
            let a0 = state.registers.get(Register::A0);
    
            if a0 == 0 {
              println!("Test {} passed!", testnum);
              machine.halt();
            } else {
              panic!("Test {} failed with a0={}", testnum, a0);
            }
          },

          _ => unimplemented!("Unknown syscall {:?}", syscall)
        }
      },
    }
  }
}