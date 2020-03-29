mod runner;

use crate::runner::run_test_suite;

macro_rules! deftest {
  ($name: ident) => {
    paste::item! {
      #[test]
      fn [<rv64um_p_ $name>] () {
        let file = include_bytes!(concat!("riscv-tests/isa/rv64um-p-", stringify!($name)));
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
      fn [<rv64um_p_ $name>] () {
        let file = include_bytes!(concat!("riscv-tests/isa/rv64um-p-", stringify!($name)));
        run_test_suite(file)
      }
    }
  }
}

deftest!(div);
deftest!(divu);
deftest!(divuw);
deftest!(divw);
deftest!(mul);
deftest!(mulh);
deftest_ignore!(mulhsu);
deftest!(mulhu);
deftest!(mulw);
deftest_ignore!(rem);
deftest_ignore!(remu);
deftest_ignore!(remuw);
deftest_ignore!(remw);
