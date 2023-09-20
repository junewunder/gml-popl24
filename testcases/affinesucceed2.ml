let f x =
  let _ = future (force x) in
  force x
;;
