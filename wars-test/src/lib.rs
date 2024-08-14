#[no_mangle]
pub fn add(left: usize, right: usize) -> usize {
    left + right
}
#[no_mangle]
pub fn memory_op(a: &str) -> String {
    return a.to_owned();
}
