type 'a mylist = MyNil
               | MyCons of 'a * 'a mylist
;;

let rec sum l =
  match l with
  | MyNil -> 0
  | MyCons (h, t) -> h + (sum t)
;;
