
let rec map fxs =
  let (f, xs) = fxs in
  match xs with
  | [] -> []
  | x :: xs ->
    let y = future (f x) in
    let ys = (map (f, xs)) in
    (force y) :: ys
;;
