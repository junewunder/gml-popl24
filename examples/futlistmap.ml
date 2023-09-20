type 'a futlist =
  | FNil
  | FCons of 'a future * 'a futlist
;;

let mapfutlist g =
  let rec mfl l =
    match l with
    | FNil -> 0
    | FCons (f, fs) -> (g f) + (mfl fs)
  in
  mfl
;;
