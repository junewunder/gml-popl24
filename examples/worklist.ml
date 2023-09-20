type 'a futlist =
  | FNil
  | FCons of 'a future * 'a futlist
;;

let rec mk_fut_list fs = match fs with
| [] -> FNil
| f :: fs -> FCons (future (f ()), mk_fut_list fs)
;;

let rec touch_fut_list xs = match xs with
| FNil -> []
| FCons (x, xs) -> (force x) :: (touch_fut_list xs)
;;

(* type 'a futlist =
  | FNil
  | FCons of 'a future * 'a futlist
;;

let rec mk_fut_list fs = match fs with
| [] -> FNil
| f :: fs -> FCons ((future f ()), mk_fut_list fs)
;; *)