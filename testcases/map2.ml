
let rec map2 fxs =
  let (f, xs) = fxs in
  match xs with
  | [] -> []
  | x :: xs ->
    let y = future (f x) in
    let ys = (map2 (f, xs)) in
    (force y) :: ys
;;
