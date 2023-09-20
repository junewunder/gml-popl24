let spawn2 x =
  let f1 = future (let f = future 42 in
                   let a = 10 in
                   let b = force f in
                   a + b)
  in
  let f2 = future (let f = future 42 in
                   let a = 10 in
                   let b = force f in
                   a + b) in
  let b = force f2 in
  let a = force f1 in
  a + b
;;
