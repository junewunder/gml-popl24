let rec mk_fut_list l =
  match l with
  | [] -> []
  | f::t -> (future (f ()))::(mk_fut_list t)
;;
