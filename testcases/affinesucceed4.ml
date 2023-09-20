let rec f x =
  if x = 0 then future 0
  else
    let ff = (f (x - 1)) in
    let _ = force ff in
    future (1 + (force ff))
;;
