type ftree =
| Empty
| Node of int * ftree future * ftree future
;;

let rec bst lohi =
  let (lo, hi) = lohi in
  if lo >= hi then Empty
  else
    let mid = (lo + hi) / 2 in
    Node (mid, future (bst (lo, mid)), future (bst (mid, hi)))
;;

let rec tree_sum tree =
  match tree with
  | Empty -> 0
  | Node (x, l, r) ->
     let left_sum_fut = future (tree_sum (force l)) in
     let right_sum = tree_sum (force r) in
     let left_sum = force left_sum_fut in
     x + left_sum + right_sum
;;

let rec reverse tree =
  match tree with
  | Empty -> Empty
  | Node (x, l, r) ->
     Node (x, future (reverse (force r)), future (reverse (force l)))
;;

let tree = bst (0, 10) in
    reverse tree
;;
