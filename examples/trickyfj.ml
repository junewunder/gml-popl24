let spawn f =
  future (f ())
;;

let spawn2 x =
  let f1 = spawn (fun x -> let f = spawn (fun x -> 42) in
                           let a = 10 in
                           let b = force f in
                           a + b)
  in
  let f2 = spawn (fun x -> 24) in
  let b = force f2 in
  let a = force f1 in
  a + b
;;
