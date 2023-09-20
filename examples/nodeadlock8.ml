let rec f futn : int list =
  let (fut, n) = futn in
  if n = 0 then
    (force !fut)::[]
  else
    (force !fut)::(f ((ref (future 42)), n - 1))
;;
