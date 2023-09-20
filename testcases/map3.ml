
let rec map3 fxs =
  let (f, xs) = fxs in
  match xs with
  | [] -> []
  | x :: xs ->
    let y = future (f x) in
    (force y) :: (map3 (f, xs))
;;
