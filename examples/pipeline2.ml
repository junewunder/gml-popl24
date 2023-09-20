let rec f n =
  if n <= 0 then 0
  else f (n - 1)
;;

let rec pipeline ffl =
  let (fut, l) = ffl in
  match l with
  | [] -> force fut
  | h::t ->
     let fprefix = future (f h + (force fut)) in
     pipeline (fprefix, t)
;;
