let rec partition pl =
  let (p, l) = pl in
  match l with
  | [] -> ([], [])
  | h::t ->
     let (a, b) = partition (p, t) in
     if h < p then
       (h::a, b)
     else
       (a, h::b)
;;

let rec append ls =
  let (l1, l2) = ls in
  match l1 with
  | [] -> l2
  | h::t -> h::(append (t, l2))
;;

let rec qsort (l: int list) =
  match l with
  | [] -> []
  | p::t ->
     (match t with
      | [] -> l
      | h::t2 ->
         let (le, gt) = partition (p, t) in
         let lef = future (qsort le) in
         let gtf = qsort gt in
         append (force lef, append (p::[], gtf)))
;;
