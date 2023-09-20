let touch_flist = fun l ->
  match l with
  | [[]] -> 0
  | h:::t -> 1 + force h
;;
