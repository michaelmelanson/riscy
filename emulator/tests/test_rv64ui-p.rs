mod runner;

use crate::runner::run_test_suite;

#[test]
fn test_rv64ui_p_simple() {
  let file = include_bytes!("/opt/riscv/target/share/riscv-tests/isa/rv64ui-p-simple");
  run_test_suite(file)
}