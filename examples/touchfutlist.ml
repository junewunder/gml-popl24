let rec touch_list l =
  match l with
  | [[]] -> ()
  | h:::t -> (force h; touch_list t)
;;
