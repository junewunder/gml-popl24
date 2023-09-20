let rec f (futn: int future ref * int) : int list =
  let (fut, n) = futn in
  if n = 0 then
    (force !fut)::[]
  else
    (force !fut)::(f (futref, n - 1))
;;
