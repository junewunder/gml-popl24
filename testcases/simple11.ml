let rec f x =
  if x = 0 then []
  else (future 1) :: (f (x - 1))
;;

let rec g xs =
  match xs with
  | [] -> []
  | y :: ys -> (future (force y)) :: (g ys)
;;

let _ =
  let xs = f 3 in
  let xs = g xs in
  xs
;;