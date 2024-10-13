wars_macro::wars!(
    file = "target/wasm32-unknown-unknown/debug/wars_test.wasm",
    r#async = true,
);

fn x<A: Wars>() {}
/*  */
