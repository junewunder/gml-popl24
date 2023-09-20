let f _ =
  let x = futref in
  let _ = x := (future (1 + 2)) in
  force (!x)
;;
            
