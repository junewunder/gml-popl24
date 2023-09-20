type intfutlist =
  | FNil
  | FCons of int future * intfutlist
;;

let rec f xs =
  match xs with
  | FNil -> []
  | FCons(y, ys) ->
    let _ = force y in
    (force y) :: (f ys)
;;
