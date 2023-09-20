type ab = A of int
        | B of string
;;

let f arg =
  match arg with
  | A x -> "int"
  | B s -> s
;;
