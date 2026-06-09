//! Auto-generated from build.rs

mod common;
use common::run_test;

#[test]
fn hello_world() {
    run_test("tests/samples/hello_world");
}

#[test]
fn table() {
    run_test("tests/samples/table");
}

#[test]
fn unicode() {
    run_test("tests/samples/unicode");
}
