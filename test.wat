(module
  (memory 1)
  (export "memory" (memory 0))
  (func $fib (param $p0 i64) (result i64)
    (if (result i64)
      (i64.lt_s
        (local.get $p0)
        (i64.const 2)
      )
      (then
        (local.get $p0)
      )
      (else
        (i64.add
          (call $fib
            (i64.sub
              (local.get $p0)
              (i64.const 1)
            )
          )
          (call $fib
            (i64.sub
              (local.get $p0)
              (i64.const 2)
            )
          )
        )
      )
    )
  )
  (func $main (result i64)
    (call $fib
      (i64.const 30)
    )
  )
  (export "_start" (func $main))
)
