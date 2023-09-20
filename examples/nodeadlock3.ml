let func _ =
  let rec force_after (fn: unit future ref * int) =
    let (f, n) = fn in
    if n <= 0 then force !f
    else force_after (f, n - 1)
  in
  let f : unit future ref = futref in
  let myspawn fr =
    fr := future ()
  in
  future (myspawn f);
  force_after (f, 5)
;;
