(* let _ = fun x -> x;; *)
let _ =
  let rec f x =
    if x = 0 then 0
    else 1 + (f (x - 1))
  in f;;


