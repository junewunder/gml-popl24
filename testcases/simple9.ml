type myintfutlist =
  | MyNil
  | MyCons of int future * myintfutlist
;;

let rec f x =
  if x = 0 then MyNil
  else MyCons (future 1, f (x - 1))
;;

let rec g xs =
  match xs with
  | MyNil -> MyNil
  | MyCons (y, ys) -> MyCons (y, g ys)
;;

let h a =
  let xs = f 3 in
  let xs = g xs in
  xs
;;
