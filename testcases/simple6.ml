let rec f x =
  if x = 0 then []
  else (1 :: (f (x - 1)))
;;
