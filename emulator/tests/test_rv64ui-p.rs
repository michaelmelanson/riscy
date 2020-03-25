mod runner;

use crate::runner::run_test_suite;

macro_rules! deftest {
  ($name: ident) => {
    paste::item! {
      #[test]
      fn [<rv64ui_p_ $name>] () {
        let file = include_bytes!(concat!("riscv-tests/isa/rv64ui-p-", stringify!($name)));
        run_test_suite(file)
      }
    }
  }
}

deftest!(add);
deftest!(addi);
deftest!(addiw);
deftest!(addw);
deftest!(and);
deftest!(andi);
deftest!(auipc);
deftest!(beq);
deftest!(bge);
deftest!(bgeu);
deftest!(blt);
deftest!(bltu);
deftest!(bne);
deftest!(fence_i);
deftest!(jal);
deftest!(jalr);
deftest!(lb);
deftest!(lbu);
deftest!(ld);
deftest!(lh);
deftest!(lhu);
deftest!(lui);
deftest!(lw);
deftest!(lwu);
deftest!(or);
deftest!(ori);
deftest!(sb);
deftest!(sd);
deftest!(sh);
deftest!(simple);
deftest!(sll);
deftest!(slli);
deftest!(slliw);
deftest!(sllw);
deftest!(slt);
deftest!(slti);
deftest!(sltiu);
deftest!(sltu);
deftest!(sra);
deftest!(srai);
deftest!(sraiw);
deftest!(sraw);
deftest!(srl);
deftest!(srli);
deftest!(srliw);
deftest!(srlw);
deftest!(sub);
deftest!(subw);
deftest!(sw);
deftest!(xor);
deftest!(xori);
