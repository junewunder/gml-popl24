let rec f n =
  if n = 0 then
    future ()
      (* (force fut)::[] *)
  else
    let x = future () in
    f (n - 1)
(* callf (future () : unit future) *)
(* (force fut)::(callf (future ())) *)
;;
