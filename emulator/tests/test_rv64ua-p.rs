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
    };
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
    };
}

deftest!(amoadd_d);
deftest!(amoadd_w);
deftest!(amoand_d);
deftest!(amoand_w);
deftest!(amomax_d);
deftest!(amomax_w);
deftest!(amomaxu_d);
deftest!(amomaxu_w);
deftest!(amomin_d);
deftest!(amomin_w);
deftest!(amominu_d);
deftest!(amominu_w);
deftest!(amoor_d);
deftest!(amoor_w);
deftest!(amoswap_d);
deftest!(amoswap_w);
deftest!(amoxor_d);
deftest!(amoxor_w);
deftest_ignore!(lrsc);
