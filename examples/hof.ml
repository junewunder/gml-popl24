let f myforce =
  let f1 = future 42 in
  let f2 = future 24 in
  (myforce f1) + (myforce f2)
;;

let myforce f = force f
;;

f myforce
;;
