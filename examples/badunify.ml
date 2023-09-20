let spawn2 fs =
  let (f1, f2) = fs in
  (future (f1 ()))::(future (f2 ()))::[]
;;
