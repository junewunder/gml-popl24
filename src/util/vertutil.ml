open Ast
open Vertex
open UnionFind

let min_bv = (bound_vertex_prefix ^ "0")
let rec max_bv_in_e e =
  match e.edesc with
  | EVar _ | EConst _ -> min_bv
  | EFun (_, uf, ut, e) ->
     max_bv uf (max_bv ut (max_bv_in_e e))
  | ERef e
    | EDeref e
    | EFuture (_, e)
    | EForce e
    | EConstructor (_, e)
    | EAnnot (e, _) -> max_bv_in_e e
  | EInfixop (_, e1, e2)
    | ELet (_, _, e1, e2)
    | ELetFun (_, _, _, _, _, _, _, e1, e2)
    | ELetTuple (_, e1, e2)
    | ELetRecord (_, e1, e2)
    | EApp (e1, _, _, e2)
    | EUpdate (e1, e2)
    | EPar (e1, e2) -> max_bv (max_bv_in_e e1) (max_bv_in_e e2)
  | EIf (e1, e2, e3) ->
     max_bv (max_bv (max_bv_in_e e1) (max_bv_in_e e2)) (max_bv_in_e e3)
  | ETry (e, cases)
    | EMatch (e, cases) ->
     max_bv (max_bv_in_e e)
       (List.fold_left max_bv min_bv
          (List.map (fun (_, _, e) -> max_bv_in_e e) cases))
  | ETuple es -> List.fold_left max_bv min_bv
                   (List.map max_bv_in_e es)
  | ENewVert (vs, _, e) ->
     max_bv vs (max_bv_in_e e)

let next_bv is_spawn bv =
  match scan_bv (keyof bv) with
  | Some n -> vert_of_string
                is_spawn
                (bound_vertex_prefix ^ (string_of_int (n + 1)))
  | None -> raise (Invalid_argument "next_bv")
