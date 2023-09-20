let rec f futn =
  let (fut, n) = futn in
  let callf ft = f (ft, n - 1) in
  if n = 0 then
    fut
      (* (force fut)::[] *)
  else
    f (future (), n - 1)
(* callf (future () : unit future) *)
(* (force fut)::(callf (future ())) *)
;;
