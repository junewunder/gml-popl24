let rec map1 fxs =
  let (f, xs) = fxs in
  match xs with
  | [] -> []
  | x :: xs -> (f x) :: (map1 (f, xs))
;;