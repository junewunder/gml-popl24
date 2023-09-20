let rec f x =
  if x = 0 then future 0
  else future (1 + (force (f (x - 1))))
;;
