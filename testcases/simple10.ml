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
  | MyNil -> 0
  | MyCons (x, xs') ->
    let y = force x in
    let sum = g xs' in
    y + sum
;;

let _ =
  let xs = f 3 in
  let x = g xs
  in x
;;
