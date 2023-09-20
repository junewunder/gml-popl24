let f (fs: (unit -> 'a) * (unit -> 'b)) =
  let (f1, f2) = fs in
  let a = future (let b = future (f2 ())
                  in f1 (); b)
  in
  let c = force a in
  force c
;;
