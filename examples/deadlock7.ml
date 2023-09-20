let func _ =
  let rec force_after (fn: unit future ref * int) =
    let (f, n) = fn in
    if n <= 0 then force !f
    else force_after (f, n - 1)
  in
  let f : unit future ref = futref in
  let rec spawn_after frn =
    let (fr, n) = frn in
    if n <= 0 then
      fr := future ()
    else spawn_after (fr, n - 1)
  in
  future (spawn_after (f, 10));
  force_after (f, 5)

;;
