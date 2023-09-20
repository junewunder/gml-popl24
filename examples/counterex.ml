let rec f (arg: int future ref * int future ref) =
  let (a, b(*, c, d*)) = arg in
  let _ = (force (!a)) + 1 in
  let newf = futref in
  let _ = f ((*c, d,*) newf, newf) in
  let _ = b := (future 1) in
  ()
;;

let main x =
  let a : int future ref = futref in
  let b : int future ref = futref in
(*
let c = futref : unit future ref;;
let d = futref : unit future ref;;
 *)
  let _ = a := (future 1) in
  (* let _ = c := future ();; *)
  let _ = f (a, b(*, c, d*)) in
  let _ = force (!b) in
  ()
;;
(* let _ = force !d;; *)
