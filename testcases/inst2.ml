type 'a futlist =
  | FNil
  | FCons of 'a future * 'a futlist
;;

let rec touch_fut_list xs = match xs with
| FNil -> []
| FCons (x, xs) -> (force x) :: (touch_fut_list xs)
;;