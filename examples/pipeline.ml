let rec pipeline ffl =
  let (f, fut, l) = ffl in
  match l with
  | [] -> force fut
  | h::t ->
     let fprefix = future (f h + (force fut)) in
     pipeline (f, fprefix, t)
;;
