mod runner;

use crate::runner::run_test_suite;

macro_rules! deftest {
  ($name: ident) => {
    paste::item! {
      #[test]
      fn [<rv64mi_p_ $name>] () {
        let file = include_bytes!(concat!("riscv-tests/isa/rv64ua-p-", stringify!($name)));
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
      fn [<rv64mi_p_ $name>] () {
        let file = include_bytes!(concat!("riscv-tests/isa/rv64ua-p-", stringify!($name)));
        run_test_suite(file)
      }
    }
  }
}

deftest!(amoadd_d);
deftest!(amoadd_w);
deftest!(amoand_d);
deftest!(amoand_w);
deftest!(amomax_d);
deftest!(amomax_w);
deftest_ignore!(amomaxu_d);
deftest_ignore!(amomaxu_w);
deftest!(amomin_d);
deftest!(amomin_w);
deftest_ignore!(amominu_d);
deftest_ignore!(amominu_w);
deftest_ignore!(amoor_d);
deftest_ignore!(amoor_w);
deftest_ignore!(amoswap_d);
deftest_ignore!(amoswap_w);
deftest_ignore!(amoxor_d);
deftest_ignore!(amoxor_w);
deftest_ignore!(lrsc);