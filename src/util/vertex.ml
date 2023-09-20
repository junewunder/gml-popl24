open UnionFind

type gvert = bool * string UnionFind.t (* is spawn position? *)

let vname (_, k) = keyof k
let classname (_, k) = findkey k
let visspawn (b, _) = b

let mk_not_spawn (_, k) = (false, k)

module S =
  struct
    type t = string
    let compare = String.compare
  end

module VMap = Map.Make(S)

let keys : gvert VMap.t ref = ref VMap.empty

let vert_of_string is_spawn s =
  match VMap.find_opt s !keys with
  | Some k -> k
  | None ->
     let k = newkey s in
     keys := VMap.add s (is_spawn, k) !keys;
     (is_spawn, k)

let veq (_, v1) (_, v2) =
  keyof (find v1) = keyof (find v2)

let bound_vertex_prefix = "ub"
let global_vertex_prefix = "u"

let gv_ctr = ref 0
let new_global_vertex () =
  let _ = gv_ctr := !gv_ctr + 1 in
  global_vertex_prefix ^ (string_of_int !gv_ctr)

let scan_bv a =
  try
    Scanf.sscanf a "ub%d" (fun n -> Some n)
  with Scanf.Scan_failure _ -> None

let rec max_bv a b =
  match (scan_bv a, scan_bv b) with
  | (None, _) -> b
  | (_, None) -> a
  | (Some n1, Some n2) -> if n1 < n2 then b else a

let rec max_bvrr a b =
  let (a, b) = (a, UnionFind.keyof b) in
  match (scan_bv a, scan_bv b) with
  | (None, _) -> b
  | (_, None) -> a
  | (Some n1, Some n2) -> if n1 < n2 then b else a

let rec max_bvbr a b =
  let (a, b) = (UnionFind.keyof a, UnionFind.keyof b) in
  match (scan_bv a, scan_bv b) with
  | (None, _) -> b
  | (_, None) -> a
  | (Some n1, Some n2) -> if n1 < n2 then b else a

let rec vmem v l =
  List.exists (fun v' -> veq v v') l

let rec vdedup l =
  match l with
  | [] -> []
  | h::t -> h::(List.filter (fun x -> not (veq x h)) t)

let diff l1 l2 =
  List.filter (fun x -> not (vmem x l2)) l1
