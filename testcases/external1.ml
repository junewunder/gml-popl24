
external foo : int -> int ;;
external bar : 'a -> 'a ;;

type hello;;

external greet : int -> hello ;;

(* external Bop.beep : 'a -> 'a;; *)

let aaa = foo 1 ;;
(* let bbb = Bop.beep true;; *)
let ccc = greet 3 ;;