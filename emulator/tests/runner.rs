use riscy_emulator::{
    machine::{RiscvMachine, RiscvMachineError},
    memory::{Memory, Permissions, Region},
    subsystem::{Subsystem, SubsystemAction},
};
use riscy_isa::Register;

#[derive(Default)]
struct TestRunnerSubsystem;

impl Subsystem for TestRunnerSubsystem {
    fn system_call(
        &mut self,
        machine: &mut RiscvMachine<Self>,
    ) -> Result<Option<SubsystemAction>, RiscvMachineError> {
        let state = machine.state();
        let syscall = state.registers.get(Register::A7);

        match syscall {
            93 => {
                let testnum = state.registers.get(Register::GlobalPointer);
                let a0 = state.registers.get(Register::A0);

                if a0 == 0 {
                    println!("Test {} passed!", testnum);
                    machine.halt();
                    Ok(None)
                } else {
                    let testnum = testnum >> 1;
                    panic!("Test {} failed", testnum);
                }
            }

            _ => unimplemented!("Unknown syscall {:?}", syscall),
        }
    }
}

pub fn run_test_suite(file: &[u8]) {
    let _ = simple_logger::init();

    let binary = goblin::elf::Elf::parse(file).expect("Could not parse file");

    let mut region = Region::readwrite_memory(0, 1024 * 1024 * 1024 * 4);

    for ph in binary.program_headers {
        // println!("Loading header: {:?}", ph);
        let header_bytes = &file[ph.file_range()];

        for (offset, byte) in header_bytes.iter().enumerate() {
            let address = (ph.p_vaddr as usize) + (offset as usize);
            region
                .write(address as u64, *byte)
                .expect("write to memory");
        }
    }

    let region = region.change_permissions(Permissions::custom(true, true));

    let mut memory = Memory::new();
    memory.add_region(region);

    let mut machine = RiscvMachine::<TestRunnerSubsystem>::new(memory, binary.header.e_entry);
    machine
        .state_mut()
        .registers
        .set(Register::StackPointer, 0x1000);
    while !machine.halted() {
        let _ = machine.step().expect("Machine step failed");
    }
}
