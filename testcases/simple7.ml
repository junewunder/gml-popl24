type myintfutlist =
  | MyNil
  | MyCons of int future * myintfutlist
;;

let rec f x =
  if x = 0 then MyNil
  else MyCons (future 1, f (x - 1))
;;
