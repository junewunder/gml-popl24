type 'a futlist =
| FNil
| FCons of 'a future * 'a futlist
;;

let rec map5 fxs =
  let (f, xs) = fxs in
  match xs with
  | [] -> FNil
  | x :: xs -> FCons (future (f x), (map5 (f, xs)))
;;