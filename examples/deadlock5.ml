let f _ =
  let fut : int future ref = futref in
  force !fut
;;
