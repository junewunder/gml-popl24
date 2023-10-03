let rec fib n =                                                                                                                      │
  if n <= 1 then 1                                                                                                                   │
  else                                                                                                                               │
    let fa = future (fib (n - 1)) in                                                                                                 │
    let b = fib (n - 2) in                                                                                                           │
    let a = force fa in                                                                                                              │
    a + b                                                                                                                            │
;;