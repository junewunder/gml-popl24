type 'a t_int = N of ('a * 'a t_int option ref)
let newkey v = N (v, ref None)

let rec find ((N (_v, r)) as k) =
  match !r with
  | None -> k
  | Some k' ->
    let k'' = find k' in
    r := Some k'';
    k''

let keyof (N (v, _)) = v

let findkey k = keyof (find k)

let union (v1: 'a t_int) (v2: 'a t_int) =
  if findkey v1 = findkey v2 then ()
  else
  let N (_, r) = find v1 in
  r := Some v2

type 'a t = 'a t_int
              [@equal fun k1 k2 -> findkey k1 = findkey k2]
              [@polyprinter
                  fun a_printer fmt k -> fprintf fmt "%a" a_printer (findkey k)]

              [@@deriving show, eq]
