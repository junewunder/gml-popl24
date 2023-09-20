let f myforce =
  let callmf ft = myforce ft in
  let f1 = future 42 in
  let f2 = future 24 in
  (callmf f1) + (callmf f2)
;;

let myforce f = force f
;;

f myforce
;;
