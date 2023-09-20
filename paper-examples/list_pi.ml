type 'a futlist =
  | FNil
  | FCons of 'a future * 'a futlist
;;

let expensive xy = let (x, y) = xy in x + y;;

let rec list_pi ak =
  let (a, k) = ak in
  let a' = future (expensive (force a, k)) in
  FCons (a', list_pi (a', k + 1))
;;

let main _ =
  let xs = list_pi (future 0, 1) in
  match xs with
  | FNil -> 1
  | FCons (_, xs) ->
     (match xs with
      | FNil -> 1
      | FCons (x, _) -> force x
     )
;;

let _ = main ()
;;
