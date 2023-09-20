open Ast

exception VisitorImpossible

let err _ = raise VisitorImpossible
let id x = x

let rec transform_const
  (f : 'a -> 'b)
  (const : 'a const)
  = match const with
  | Num _
  | String _
  | Char _
  | Bool _
  | Unit -> const
  | Futref x -> Futref (f x)

(* let rec transform_vs_typ
  (f1 : 't_unif1 -> 't_unif2)
  (f2 : 'graph1 -> 'graph2)
  (f3 : 'vert1 -> 'vert2)
  (f4 : 'vert_param1 -> 'vert_param2)
  (typ : ('t_unif1, 'graph1, 'vert1, 'vert_param1) vs_typ) =
  let rebuild tdesc = { typ with tdesc } in
  let recur = transform_vs_typ f1 f2 f3 f4 in
  rebuild @@ match typ.tdesc with
  | TVar x -> TVar x
  | TUVar x -> TUVar (f1 x)
  | TRef x -> TRef (recur x)
  | TArrow (x, y, z) -> TArrow (recur x, recur y, f2 z)
  | TPi (x, y, z) -> TPi (f4 x, f4 y, recur z)
  | TProd xs -> TProd (List.map recur xs)
  | TFuture (x, y) -> TFuture (f3 x, recur y)
  | TConstr (x, y, z) -> TConstr (x, List.map recur y, f3 z)
  | TRecType xs -> TRecType (List.map (fun (x, y) -> (x, recur y)) xs) *)

let rec transform_vs_typ
          (f1 : 'g_unif1 -> 'g_unif2)
          (vst: 'g_unif vs_typ) =
  match vst with
  | VSTVertex -> VSTVertex
  | VSTVar t -> VSTVar t
  | VSTUVar i -> VSTUVar (f1 i)
  | VSTProd vsts ->
     VSTProd (List.map (transform_vs_typ f1) vsts)
  | VSTCoRec (t, vst) -> VSTCoRec (t, transform_vs_typ f1 vst)

let rec transform_vertex_struct
  (f1 : 'g_unif1 -> 'g_unif2)
  (f2 : 'vs_typ1 -> 'vs_typ2)
  (x : 'g_unif1 vertex_struct) =
  let recur = transform_vertex_struct f1 f2 in
  match x with
  | VSVar x -> VSVar x
  | VSUVar x -> VSUVar (f1 x)
  | VSTuple x -> VSTuple (List.map recur x)
  | VSProj (x, y, n) -> VSProj (recur x, y, f2 n)

let rec transform_graph
  (f1 : 'g_unif1 -> 'g_unif2)
  (f2 : 'g_unif1 vertex_struct -> 'g_unif2 vertex_struct)
  (f3: 'g_unif1 vs_typ -> 'g_unif2 vs_typ)
  (x : 'g_unif1 graph) =
  let recur = transform_graph f1 f2 f3 in
  match x with
  | GEmpty -> GEmpty
  | GVar x -> GVar x
  | GUVar x -> GUVar (f1 x)
  | GSeq (x, y) -> GSeq (recur x, recur y)
  | GOr (x, y) -> GOr (recur x, recur y)
  | GPar (x, y) -> GPar (recur x, recur y)
  | GFut (x, y) -> GFut (recur x, f2 y)
  | GTouch x -> GTouch (f2 x)
  | GPi (x, y, z, w, v) -> GPi (x, f3 y, z, f3 w, recur v)
  | GRec (x, y) -> GRec (x, recur y)
  | GNew (x, y, z) -> GNew (x, f3 y, recur z)
  | GApp (x, y, z) -> GApp (recur x, f2 y, f2 z)

let rec transform_typ
  (f1 : 't_unif1 -> 't_unif2)
  (f2 : 'graph1 -> 'graph2)
  (f3 : 'vert1 -> 'vert2)
  (f4 : 'vert_param1 -> 'vert_param2)
  (typ : ('t_unif1, 'graph1, 'vert1, 'vert_param1) typ) =
  let rebuild tdesc = {typ with tdesc} in
  let recur = transform_typ f1 f2 f3 f4 in
  rebuild @@ match typ.tdesc with
  | TVar x -> TVar x
  | TUVar x -> TUVar (f1 x)
  | TRef x -> TRef (recur x)
  | TArrow (x, y, z, w, q) -> TArrow (recur x, recur y, f4 z, f4 w, f2 q)
  | TProd xs -> TProd (List.map recur xs)
  | TFuture (x, y) -> TFuture (f3 x, recur y)
  | TConstr (x, y, z) -> TConstr (x, List.map recur y, f3 z)
  | TRecType xs -> TRecType (List.map (fun (x, y) -> x, recur y) xs)

let rec transform_schema
  (f1 : ('t_unif1, 'graph1, 'vert1, 'vert_param1) typ -> ('t_unif2, 'graph2, 'vert2, 'vert_param2) typ)
  (sch : g_schema)  =
  let recur = transform_schema f1 in
  match sch with
  | SMono t -> SMono (f1 t)
  | SForall (x, sch) -> SForall (x, recur sch)
  | SForallG (x, sch) -> SForallG (x, recur sch)

let rec transform_expr
  (f1 : 't_unif1 -> 't_unif2)
  (f2 : 'g_unif1 -> 'g_unif2)
  (f3 : 'typ1 -> 'typ2)
  (f4 : 'graph1 -> 'graph2)
  (f5 : 'vert1 -> 'vert2)
  (f6 : 'vert_param1 -> 'vert_param2)
  (f7 : 'g_unif1 vs_typ -> 'g_unif2 vs_typ)
  (expr : ('t_unif1, 'g_unif1, 'typ1, 'graph1, 'vert1, 'vert_param1) expr) =
  let recur = transform_expr f1 f2 f3 f4 f5 f6 f7 in
  let handle_matches = List.map (fun (x, y, z) -> (x, y, recur z)) in
  let rebuild edesc =
    let etyp = f3 expr.etyp in
    let egr = f4 expr.egr in
    {expr with edesc; etyp; egr} in
  rebuild @@ match expr.edesc with
  | EVar x -> EVar x
  | EConst x -> EConst (transform_const f5 x)
  | EInfixop (x, y, z) -> EInfixop (x, recur y, recur z)
  | EIf (x, y, z) -> EIf (recur x, recur y, recur z)
  | ELet (x, y, z, w) -> ELet (x, y, recur z, recur w)
  | EFunc (a, b, c, d, e, f, g, h) -> EFunc (a, b, c, d, e, f6 f, f6 g, recur h)
  | ELetTuple (x, y, z) -> ELetTuple (x, recur y, recur z)
  | ELetRecord (x, y, z) -> ELetRecord (x, recur y, recur z)
  | EApp (x, y, z, w) -> EApp (recur x, f5 y, f5 z, recur w)
  | EMatch (x, y) -> EMatch (recur x, handle_matches y)
  | ETuple x -> ETuple (List.map recur x)
  | ERef x -> ERef (recur x)
  | EDeref x -> EDeref (recur x)
  | EUpdate (x, y) -> EUpdate (recur x, recur y)
  | EFuture (x, y) -> EFuture (f5 x, recur y)
  | EForce x -> EForce (recur x)
  | EPar (x, y) -> EPar (recur x, recur y)
  | ETry (x, y) -> ETry (recur x, handle_matches y)
  | EAnnot (x, y) -> EAnnot (recur x, y)
  | ENewVert (x, y, z) -> ENewVert (x, f7 y, recur z)

(* let rec transform_decl (x : ('t_unif, 'g_unif, 'typ, 'schema, 'graph, 'vert, 'vert_param) decl) = _ *)

