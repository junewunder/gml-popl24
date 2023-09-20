let func _ =
  let f = futref in
  let x: unit = force !f in
  f := future ()
;;
