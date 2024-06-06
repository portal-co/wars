wars_macro::wars!(
    inline = "
    (module
        (func (result i32) call 2 call 1)
        (func (result i32)
        i32.const 0
        (if
            (then i32.const 0
            return_call 0
            )
            (else i32.const 1)
        )
              )
              (func)
            )
      
    ",
);
