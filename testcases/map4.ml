
let rec map4 fxs =
  let (f, xs) = fxs in
  match xs with
  | [] -> []
  | x :: xs ->
    let y = future (f x) in
    let ys = future (map4 (f, xs)) in
    (force y) :: (force ys)
;;
