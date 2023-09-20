type 'a mylist = MyNil
               | MyCons of 'a * 'a mylist
;;

let rec length l =
  match l with
  | MyNil -> 0
  | MyCons (h, t) -> 1 + (length t)
;;

let a = 5;;
let b = 10;;
