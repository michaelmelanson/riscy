mod runner;

use crate::runner::run_test_suite;

macro_rules! deftest {
    ($name: ident) => {
        paste::item! {
          #[test]
          fn [<rv64mi_p_ $name>] () {
            let file = include_bytes!(concat!("riscv-tests/isa/rv64uc-p-", stringify!($name)));
            run_test_suite(file)
          }
        }
    };
}

// macro_rules! deftest_ignore {
//   ($name: ident) => {
//     paste::item! {
//       #[test]
//       #[ignore]
//       fn [<rv64um_p_ $name>] () {
//         let file = include_bytes!(concat!("riscv-tests/isa/rv64uc-p-", stringify!($name)));
//         run_test_suite(file)
//       }
//     }
//   }
// }

deftest!(rvc);
