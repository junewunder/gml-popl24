let rec f a =
  let callf a = f a in
  if true then
    a
  else
    callf (future ())
;;
