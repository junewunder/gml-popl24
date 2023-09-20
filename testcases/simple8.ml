type myintfutlist =
  | MyNil
  | MyCons of int future * myintfutlist
;;

let rec f xs =
  match xs with
  | MyNil -> MyNil
  | MyCons (y, ys) -> MyCons (y, f ys)
;;
