let rec rev al =
  let (a, l) = al in
  match l with
  | [] -> a
  | h::t -> rev (h::a, t)
;;
