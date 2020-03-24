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

macro_rules! deftest_ignore {
  ($name: ident) => {
    paste::item! {
      #[test]
      #[ignore]
      fn [<rv64ui_p_ $name>] () {
        let file = include_bytes!(concat!("/opt/riscv/target/share/riscv-tests/isa/rv64ui-p-", stringify!($name)));
        run_test_suite(file)
      }
    }
  }
}

deftest!(add);
deftest!(addi);
deftest!(addiw);
deftest_ignore!(addw);
deftest!(and);
deftest!(andi);
deftest!(auipc);
deftest!(beq);
deftest!(bge);
deftest!(bgeu);
deftest!(blt);
deftest!(bltu);
deftest!(bne);
deftest_ignore!(fence_i);
deftest!(jal);
deftest!(jalr);
deftest_ignore!(lb);
deftest_ignore!(lbu);
deftest!(ld);
deftest_ignore!(lh);
deftest_ignore!(lhu);
deftest_ignore!(lui);
deftest_ignore!(lw);
deftest_ignore!(lwu);
deftest!(or);
deftest!(ori);
deftest_ignore!(sb);
deftest!(sd);
deftest_ignore!(sh);
deftest!(simple);
deftest!(sll);
deftest!(slli);
deftest!(slliw);
deftest_ignore!(sllw);
deftest!(slt);
deftest_ignore!(slti);
deftest_ignore!(sltiu);
deftest!(sltu);
deftest_ignore!(sra);
deftest_ignore!(srai);
deftest!(sraiw);
deftest_ignore!(sraw);
deftest!(srl);
deftest!(srli);
deftest_ignore!(srliw);
deftest_ignore!(srlw);
deftest!(sub);
deftest_ignore!(subw);
deftest_ignore!(sw);
deftest!(xor);
deftest!(xori);