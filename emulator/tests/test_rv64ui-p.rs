mod runner;

use crate::runner::run_test_suite;

macro_rules! deftest {
  ($name: ident) => {
    #[test]
    fn $name() {
      let file = include_bytes!(concat!("/opt/riscv/target/share/riscv-tests/isa/rv64ui-p-", stringify!($name)));
      run_test_suite(file)
    }
  }
}

macro_rules! deftest_ignore {
  ($name: ident) => {
    #[test]
    #[ignore]
    fn $name() {
      let file = include_bytes!(concat!("/opt/riscv/target/share/riscv-tests/isa/rv64ui-p-", stringify!($name)));
      run_test_suite(file)
    }
  }
}

deftest_ignore!(add);
deftest_ignore!(addi);
deftest_ignore!(addiw);
deftest_ignore!(addw);
deftest_ignore!(and);
deftest_ignore!(andi);
deftest_ignore!(auipc);
deftest!(beq);
deftest_ignore!(bge);
deftest!(bgeu);
deftest_ignore!(blt);
deftest!(bltu);
deftest!(bne);
deftest_ignore!(fence_i);
deftest_ignore!(jal);
deftest_ignore!(jalr);
deftest_ignore!(lb);
deftest_ignore!(lbu);
deftest_ignore!(ld);
deftest_ignore!(lh);
deftest_ignore!(lhu);
deftest_ignore!(lui);
deftest_ignore!(lw);
deftest_ignore!(lwu);
deftest_ignore!(or);
deftest_ignore!(ori);
deftest_ignore!(sb);
deftest_ignore!(sd);
deftest_ignore!(sh);
deftest!(simple);
deftest_ignore!(sll);
deftest_ignore!(slli);
deftest_ignore!(slliw);
deftest_ignore!(sllw);
deftest_ignore!(slt);
deftest_ignore!(slti);
deftest_ignore!(sltiu);
deftest!(sltu);
deftest_ignore!(sra);
deftest_ignore!(srai);
deftest_ignore!(sraiw);
deftest_ignore!(sraw);
deftest_ignore!(srl);
deftest_ignore!(srli);
deftest_ignore!(srliw);
deftest_ignore!(srlw);
deftest_ignore!(sub);
deftest_ignore!(subw);
deftest_ignore!(sw);
deftest_ignore!(xor);
deftest_ignore!(xori);
