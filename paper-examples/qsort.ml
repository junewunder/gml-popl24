let rec concat args =
  let (xs, ys) = args in
  match xs with
  | [] -> ys
  | x :: xs -> x :: (concat (xs, ys))
;;

let rec partition args =
  let (xs, t) = args in
  match xs with
  | [] -> ([], [])
  | x :: xs ->
    let (lt, ge) = partition (xs, t) in
    if x < t then (x :: lt, ge)
    else (lt, x :: ge)
;;

let rec qsort l =
  match l with
  | [] -> []
  | p::t ->
    let (lt, ge) = partition (t, p) in
    let future_sort_lt = future (qsort lt) in
    let sort_ge = qsort ge in
    let sort_lt = force future_sort_lt in
    concat (sort_lt, p :: sort_ge)
;;
