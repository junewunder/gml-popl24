let rec pipeline ffl =
  let (f, fut, l) = ffl in
  let f: (int -> int) = f in
  match l with
  | [] -> force fut
  | h::t ->
     let fprefix = future ((force fut) + f h) in
     pipeline (f, fprefix, t)
;;
