type 'a pipe = Pipe of 'a * 'a pipe future;;

let expensive args = let (a, k) = args in a + k;;

let rec pipeline_pi args =
  let (a, k) = args in
  let a' = expensive (a, k) in
  Pipe (a', future (pipeline_pi (a', k + 1)))
;;

let rec nth args =
  let (pipe, n) = args in
  match pipe with
  | Pipe (a, f) ->
    if n <= 0 then a
    else nth (force f, n - 1)
;;

let main u =
  nth (pipeline_pi (0, 1), 1000)
;;