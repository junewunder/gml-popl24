let ans x =
  let f = future (20 + 1) in
  let a = 20 + 1 in
  let b = force f in
  a + b
;;

ans ();;
