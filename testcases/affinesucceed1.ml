let f x =
  let _ = force x in
  let _ = force x in
  1
;;
