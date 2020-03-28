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
deftest_ignore!(divu);
deftest_ignore!(divuw);
deftest_ignore!(divw);
deftest!(mul);
deftest_ignore!(mulh);
deftest_ignore!(mulhsu);
deftest_ignore!(mulhu);
deftest_ignore!(mulw);
deftest_ignore!(rem);
deftest_ignore!(remu);
deftest_ignore!(remuw);
deftest_ignore!(remw);
