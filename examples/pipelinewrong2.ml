let rec f n =
  if n <= 0 then 0
  else f (n - 1)
;;

let rec pipeline ffl =
  let (fut, l) = ffl in
  match l with
  | [] -> force fut
  | h::t ->
     let fprefix = future ((force fut) + f h) in
     pipeline (fprefix, t)
;;
