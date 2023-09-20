let rec f x =
  let (a, b) = x
  in
  if a = 0 then b
  else f (a - 1, b)
;;
