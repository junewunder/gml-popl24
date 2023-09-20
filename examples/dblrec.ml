let rec f n =
  let rec g n =
    if n <= 0 then 0
    else g (n - 1)
  in
  if n <= 0 then 0
  else f (n - 1) + g (n - 1)
;;
