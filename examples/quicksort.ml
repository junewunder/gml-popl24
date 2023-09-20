
let fst p = let (x, _) = p in x;;
let snd p = let (_, y) = p in y;;

let rec partition pl =
  let (p, l) = pl in
  match l with
  | [] -> ([], [])
  | h::t ->
     let (a, b) = partition (p, t) in
     if h <= p then
       (h::a, b)
     else
       (a, h::b)
;;

(* n = 0: need both a left and right head
 * n = 1: just need a left head
 * n = 2: just need a right head *)
let rec partition_part npl =
  let (n, p, l) = npl in
  match force l with
  | [] -> []
  | h::t ->
     if (n = 1 && h <= p) || (n = 2 && h > p) then
       let f = future (partition (p, t)) in
       ([], future (fst (force f)), [], future (snd (force f)))
     else if h <= p then
       let (al, af, bl, bf) = partition_part (2, p, t) in
       (h::al, af, bl, bf)
     else
       let (al, af, bl, bf) = partition_part (1, p, t) in
       (al, af, h::bl, bf)
;;

let rec append ls =
  let (l1, l2) = ls in
  match l1 with
  | [] -> l2
  | h::t -> h::(append (t, l2))
;;

let rec qsort l =
  match l with
  | [] -> []
  | [(
  | p::t ->
     (match t with
      | [] -> l
      | h::t2 ->
         let (le, gt) = partition (p, t) in
         let lef = future (qsort le) in
         let gtf = qsort gt in
         append (force lef, append (p::[], gtf)))
;;
