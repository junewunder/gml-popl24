let f x =
  let myspawn f = future (f ()) in
  let myforce f = force f in
  let f1 = myspawn (fun _ -> 42) in
  let f2 = myspawn (fun _ -> 42) in
  let f3 = myspawn (fun _ -> "Hello ") in
  let f4 = myspawn (fun _ -> "world!") in
  let a = (myforce f1) + (myforce f2) in
  let b = (myforce f3) ^ (myforce f4) in
  (a, b)
;;
