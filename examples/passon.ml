let rec f futn =
  let (fut, n) = futn in
  if n = 0 then
    (force fut)::[]
  else
    (force fut)::(f (future (), n - 1))
;;
