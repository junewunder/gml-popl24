type 'a flist =
| FNil
| FCons of 'a * ('a flist future);;

let rec produce n =
  if n < 0 then FNil
  else FCons (n, future (produce (n - 1)))
;;

let rec consume sumxs =
  let (sum, xs) = sumxs in
  match xs with
  | FNil -> sum
  | FCons (x, xs) -> consume (x + sum, force xs)
;;

let sum n = consume (0, (produce n));;

