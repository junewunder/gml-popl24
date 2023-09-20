(* external ensureInt : int -> int;;
external ensureString : string -> string;; *)
(* let id = fun x -> x ;; *)

let _ = let x = future 1 in force x;;