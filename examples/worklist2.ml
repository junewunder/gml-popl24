type 'a futlist =
  | FNil
  | FCons of 'a future * 'a futlist
;;

let rec map_f fxs =
  let (f, xs) = fxs in
  match xs with
  | [] -> FNil
  | x :: xs -> FCons ((f x), (map (f, xs)))
;;

let mk_future f = future (f ())
;;

let rec mk_fut_list fs = match fs with
| [] -> FNil
| f :: fs -> FCons (future (f ()), mk_fut_list fs)
;;

let xs = (fun u -> 1) :: ((fun u -> 2) :: ((fun u -> 3) :: []));;

let fs = map_f (mk_future, xs);;

(* let rec mk_fut_list fs = match fs with
| [] -> FNil
| f :: fs -> FCons ((future f ()), mk_fut_list fs)
;; *)