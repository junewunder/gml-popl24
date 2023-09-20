let rec f n =
  if n = 0 then future ()
  else f (n - 1)
;;
