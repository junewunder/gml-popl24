type 'a pipe = Pipe of 'a * 'a pipe future ;;

let expensive xy =
  let (x, y) = xy in
  x + y;;

let rec pipeline_pi ak =
  let (a, k) = ak in
  let a' = expensive (a, k) in
  Pipe (a', future (pipeline_pi (a', k + 1)))
;;

let main _ =
  match pipeline_pi (0, 1) with
  | Pipe (_, f1) ->
     (match force f1 with
      | Pipe (pi2, _) -> pi2)
;;

let _ = main ();;
