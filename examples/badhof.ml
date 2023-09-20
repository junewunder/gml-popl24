let f myspawn =
  let f1 = myspawn (fun x -> 42) in
  let f2 = myspawn (fun x -> 24) in
  (force f1) + (force f2)
;;

let myspawn f = future (f ())
;;

f myspawn
;;
