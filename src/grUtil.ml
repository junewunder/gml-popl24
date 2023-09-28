open Ast
open AstPrint
open Visitor
module UF = UnionFind

(* TODO: make Ctx's use longid's instead of strings *)
module GrCtx = Map.Make (String)
(* TODO: TyCtx should be over g_typs *)
module TyCtx = Map.Make (String)
module VSUTCtx = Map.Make (Int)
module VSCtx = Map.Make (
  struct
    type t = vertex_var
    let compare (VId s1) (VId s2) = String.compare (UF.keyof s1) (UF.keyof s2)
  end
)
module Subst = Map.Make (Int)

module Int_set = Set.Make
  (struct
    type t = int
    let compare = compare
  end)
let set_of_list = List.fold_left (fun acc x -> Int_set.add x acc) Int_set.empty

type gr_ctx = g_graph GrCtx.t (* graph context *)
type vs_ctx = g_vs_typ VSCtx.t (* vertex structure context *)
type vsut_ctx = g_vs_typ VSUTCtx.t (* vertex struct unif var idx -> vertex structure types *)
type gr_subst = g_graph Subst.t (* graph unif vars -> graphs *)
type vs_subst = g_vertex_struct Subst.t (* vertex struct unif vars -> vertex structures *)
type vst_subst = g_vs_typ Subst.t (* vertex struct unif vars -> vertex structures *)
type ty_ctx = g_schema TyCtx.t
type tyname_ctx = (string list * g_schema) TyCtx.t

exception CyclicSubstitutionInner
exception CyclicGrSubst of gr_subst
exception CyclicVSSubst of vs_subst
exception CyclicVSTSubst of vst_subst

(* let pp_gr_ctx (fmt : Format.formatter) (ctx : gr_ctx) =
  Format.fprintf fmt "%a" [%show: (string * ty_schema) list] (Ctx.bindings ctx) *)
let show_gr_ctx (ctx : gr_ctx) = [%show: (string * g_graph) list] (GrCtx.bindings ctx)
let show_vs_ctx (ctx : vs_ctx) = [%show: (vertex_var * g_vs_typ) list] (VSCtx.bindings ctx)
let show_gr_subst (s : gr_subst) = [%show: (int * g_graph) list] (Subst.bindings s)
let show_vs_subst (s : vs_subst) = [%show: (int * g_vertex_struct) list] (Subst.bindings s)

let rec uniq l =
      match l with
      | [] -> []
      | h::t -> h::(uniq (List.filter ((<>) h) t))
let diff a b = List.filter (fun x -> not @@ List.mem x b) a
let inter a b = List.filter (fun x -> List.mem x b) a

(* concept: a system for maintaining line numbers without
   in grcheck_state we have a field `inspection_stack`
   whenever we inspect something or do an operation where
   a monadic error may occur, we add it to the inspection stack *)

type grcheck_state = {
  inspection_stack : errorable list;
  next_graph_unif_var_num : int ref;
  next_vertex_unif_var_num : int ref;
  next_vs_typ_unif_var_num : int ref;
  next_vertex_id : int ref;
  next_graph_id : int ref;
  gr_ctx : gr_ctx [@opaque];
  vs_unif_typ_ctx : vsut_ctx ref [@opaque];
  spawn_ctx : vs_ctx  [@opaque];
  touch_ctx : vs_ctx  [@opaque];
  ty_ctx : ty_ctx [@opaque];
  tyname_ctx : tyname_ctx [@opaque];
  (* Maps constructor names to a type schema of a TArrow (arg type, ret type) *)
  con_ctx : ty_ctx [@opaque];
} [@@deriving show]

type gr_error_desc =
  | GrErrUnboundVertexVar of string
  | GrErrUnboundGraphVar of string
  | GrErrUnboundTypeCon of string
  | GrErrMismatchedBinders
  | GrErrUnimplemented of string
  | GrErrResidualTyUnifVar
  | GrErrUnknown of string
  | GrErrCannotUnifyGrs of g_graph * g_graph
  | GrErrCannotUnifyVSs of g_vertex_struct * g_vertex_struct
  | GrErrCannotUnifyVSTs of g_vs_typ * g_vs_typ
  | GrErrCannotUnifyTys of g_typ * g_typ
  [@@deriving show]

type gr_error =
  { gedesc : gr_error_desc
  ; geloc : loc option
  ; geinspection_stack : errorable list
  } [@@deriving show]

let gr_err_no_loc e = { gedesc = e; geloc = None; geinspection_stack = []}

let gr_err e loc = { gedesc = e; geloc = Some loc; geinspection_stack = []}

exception GraphError of gr_error [@@deriving show]

type 'a grcheck_result = ('a, gr_error) result [@@deriving show]

type 'a grchecker = grcheck_state -> 'a grcheck_result

let (>>=) (m : 'a grchecker) (f : 'a -> 'b grchecker) : 'b grchecker =
  fun state ->
    match m state with
    | Ok res -> f res state
    | Error err -> Error err

let (>>) m f = m >>= fun _ -> f

let (let*) = (>>=)

let return (x : 'a) : 'a grchecker = fun _ -> Ok x

let fail (err : gr_error_desc) : 'a grchecker =
  fun (state : grcheck_state) ->
    (* message "gr ctx"; *)
    (* pprint (pprint_str_bindings pprint_g_graph) (GrCtx.bindings state.gr_ctx) ; *)
    (* message "vs unif type ctx"; *)
    (* pprint (pprint_int_bindings pprint_g_vs_typ) (VSUTCtx.bindings !(state.vs_unif_typ_ctx)) ; *)
    (* state.touch_ctx *)
    Error
      { gedesc = err
      ; geloc = None
      ; geinspection_stack = state.inspection_stack
      }

let fail_with_loc (err : gr_error_desc) (loc : loc) : 'a grchecker =
  fun (state : grcheck_state) ->
    Error
      { gedesc = err
      ; geloc = Some loc
      ; geinspection_stack = state.inspection_stack
      }

let unimplemented msg = fail (GrErrUnimplemented msg)

let rec foldM f a bs = List.fold_left (fun a b -> let* a = a in f a b) (return a) bs

let rec all (ms : 'a grchecker list) : 'a list grchecker =
    match ms with
    | [] -> return []
    | m :: ms ->
      let* m = m in
      let* ms = all ms in
      return (m :: ms)

let rec mapM f l =
  match l with
  | [] -> return []
  | a::l -> let* a = f a in
            let* l = mapM f l in
            return (a::l)

let foldM f a bs = List.fold_left (fun a b -> let* a = a in f a b) (return a) bs

let rec map2M f l1 l2 =
  match l1, l2 with
  | [], _ -> return []
  | _, [] -> return []
  | a::l1, b::l2 ->
     let* c = f a b in
     let* l = map2M f l1 l2 in
     return (c::l)

let get_state : grcheck_state grchecker =
  fun state -> Ok state

let get_gr_ctx : gr_ctx grchecker =
  fun state -> Ok state.gr_ctx

let get_gr_var x : g_graph grchecker =
  let* ctx = get_gr_ctx in
  try
    let g = GrCtx.find x ctx in
    return g
  with Not_found -> fail @@ GrErrUnboundGraphVar x

let get_con_ctx : ty_ctx grchecker =
  fun state -> Ok state.con_ctx

let get_con_schema x : g_schema grchecker =
  let* ctx = get_con_ctx in
  match TyCtx.find_opt x ctx with
  | Some s -> return s
  | None -> fail @@ GrErrUnboundTypeCon x

(* let get_gr_ctx : vs_ctx grchecker =
  fun state -> Ok (!(state.vs_ctx))

let get_vs_var x : g_vs_typ grchecker =
  let* ctx = get_vs_ctx in
  try
    let g = GrCtx.find x ctx in
    return g
  with Not_found -> fail @@ GrErrUnboundGraphVar x *)

let get_vs_unif_typ_ctx : vsut_ctx grchecker =
  fun state -> Ok !(state.vs_unif_typ_ctx)

let get_ty_ctx : ty_ctx grchecker =
  fun state -> Ok state.ty_ctx

let get_tyname_ctx : tyname_ctx grchecker =
  fun state -> Ok state.tyname_ctx

let with_transformed_state f m : 'a grchecker =
  fun state -> m (f state)

let inspect (x : errorable) m : 'a grchecker =
  let transform state =
    {state with inspection_stack = x :: state.inspection_stack}
  in
  with_transformed_state transform m

let with_transformed_gr_ctx (f : gr_ctx -> gr_ctx) m =
  with_transformed_state (fun state -> { state with gr_ctx = f state.gr_ctx }) m
let with_transformed_spawn_ctx (f : vs_ctx -> vs_ctx) m =
  with_transformed_state (fun state -> { state with spawn_ctx = f state.spawn_ctx }) m
let with_transformed_touch_ctx (f : vs_ctx -> vs_ctx) m =
  with_transformed_state (fun state -> { state with touch_ctx = f state.touch_ctx }) m
let with_transformed_ty_ctx (f : ty_ctx -> ty_ctx) m =
  with_transformed_state (fun state -> { state with ty_ctx = f state.ty_ctx }) m

let with_extended_gr_ctx name gr (m : 'a grchecker) : 'a grchecker =
  with_transformed_gr_ctx (GrCtx.add name gr) m
let with_extended_spawn_ctx name gr (m : 'a grchecker) : 'a grchecker =
  with_transformed_spawn_ctx (VSCtx.add name gr) m
let with_extended_touch_ctx name gr (m : 'a grchecker) : 'a grchecker =
  with_transformed_touch_ctx (VSCtx.add name gr) m
let with_extended_ty_ctx name t (m : 'a grchecker) : 'a grchecker =
  with_transformed_ty_ctx (TyCtx.add name t) m
let rec with_extended_ty_ctx_n names ts (m : 'a grchecker) : 'a grchecker =
  match names, ts with
  | [], [] -> m
  | name :: names, t :: ts ->
    with_extended_ty_ctx name t begin
      with_extended_ty_ctx_n names ts m
    end
  | _ -> fail GrErrMismatchedBinders

let transform_vsut_ctx (f : vsut_ctx -> vsut_ctx) =
  fun state ->
    return @@ (state.vs_unif_typ_ctx := f !(state.vs_unif_typ_ctx))

let set_vs_typ_unif_var i vst : unit grchecker = fun state ->
  let ctx = !(state.vs_unif_typ_ctx) in
  let ctx = VSUTCtx.update i (fun _ -> Some vst) ctx in
  (return (state.vs_unif_typ_ctx := ctx)) state

let new_graph_unif_var : int grchecker = fun state ->
  let id = !(state.next_graph_unif_var_num) in
  state.next_graph_unif_var_num := 1 + id;
  return id state

let new_vs_typ_unif_var : int grchecker = fun state ->
  let id = !(state.next_vs_typ_unif_var_num) in
  state.next_vs_typ_unif_var_num := 1 + id;
  return id state

let new_vertex_unif_var vst : int grchecker = fun state ->
  let id = !(state.next_vertex_unif_var_num) in
  pprint (fun f () -> Format.fprintf f "creating $%d with type %a" id
                        pprint_g_vs_typ vst
    )
    ();
  state.next_vertex_unif_var_num := 1 + id;
  state.vs_unif_typ_ctx := VSUTCtx.add id vst
    !(state.vs_unif_typ_ctx) ;
  return id state




let new_vertex_name : vertex_var grchecker = fun state ->
  let id = !(state.next_vertex_id) in
  state.next_vertex_id := 1 + id;
  return (VId (UnionFind.newkey ("u" ^ string_of_int id))) state

let new_graph_var : string grchecker = fun state ->
  let id = !(state.next_graph_id) in
  state.next_graph_id := 1 + id;
  return ("g" ^ (string_of_int id)) state

(** [gr[e/x]] , or , "replace all instances of [GVar x] with [e]" *)
let rec substitute_gr_var x e (gr : g_graph) : g_graph =
  match gr with
  | GEmpty | GUVar _ -> gr
  | GVar x -> e
  | GSeq (g1, g2) -> GSeq (substitute_gr_var x e g1, substitute_gr_var x e g2)
  | GOr (g1, g2) -> GOr (substitute_gr_var x e g1, substitute_gr_var x e g2)
  | GPar (g1, g2) -> GPar (substitute_gr_var x e g1, substitute_gr_var x e g2)
  | GFut (g, v) -> GFut (substitute_gr_var x e g, v)
  | GTouch v -> GTouch v
  | GPi (uf, v1, ut, v2, g) -> GPi (uf, v1, ut, v2, substitute_gr_var x e g)
  | GRec (y, g) -> GRec (x, substitute_gr_var x e g)
  | GNew (y, v, g) -> GNew (y, v, substitute_gr_var x e g)
  | GApp (g, v1, v2) -> GApp (substitute_gr_var x e g, v1, v2)

let visit i visited =
  if (Int_set.exists ((=) i) visited)
    then raise CyclicSubstitutionInner
  else Int_set.add i visited

let rec substitute_gr s visited (gr : g_graph) : g_graph =
  match gr with
  | GEmpty | GVar _ -> gr
  | GUVar i ->
    let visited = visit i visited in
    (try
      let target = Subst.find i s in
      substitute_gr s visited target
    with Not_found -> GUVar i)
  | GSeq (g1, g2) -> GSeq (substitute_gr s visited g1, substitute_gr s visited g2)
  | GOr (g1, g2) -> GOr (substitute_gr s visited g1, substitute_gr s visited g2)
  | GPar (g1, g2) -> GPar (substitute_gr s visited g1, substitute_gr s visited g2)
  | GFut (g, v) -> GFut (substitute_gr s visited g, v)
  | GTouch v -> GTouch v
  | GPi (uf, v1, ut, v2, g) -> GPi (uf, v1, ut, v2, substitute_gr s visited g)
  | GRec (x, g) -> GRec (x, substitute_gr s visited g)
  | GNew (x, v, g) -> GNew (x, v, substitute_gr s visited g)
  | GApp (g, v1, v2) -> GApp (substitute_gr s visited g, v1, v2)
let substitute_gr s gr =
  try
    substitute_gr s Int_set.empty gr
  with CyclicSubstitutionInner -> raise @@ CyclicGrSubst s

let rec substitute_vs s (visited : Int_set.t) (vert : g_vertex_struct) : g_vertex_struct =
  match vert with
  | VSVar _ -> vert
  | VSUVar i ->
    let visited = visit i visited in
    (try
      let target = Subst.find i s in
      substitute_vs s visited target
    with Not_found -> VSUVar i)
  | VSTuple vs -> VSTuple (List.map (substitute_vs s visited) vs)
  | VSProj (v, i, n) -> VSProj (substitute_vs s visited v, i, n)
let substitute_vs s vert =
  try
    substitute_vs s Int_set.empty vert
  with CyclicSubstitutionInner -> raise @@ CyclicVSSubst s

let rec substitute_vst s visited vst : g_vs_typ =
  match vst with
  | VSTVertex -> vst
  | VSTVar _ -> vst
  | VSTUVar i ->
    let visited = visit i visited in
    (try
      let target = Subst.find i s in
      substitute_vst s visited target
    with Not_found -> VSTUVar i)
  | VSTProd vsts -> VSTProd (List.map (substitute_vst s visited) vsts)
  | VSTCoRec (x, t) -> VSTCoRec (x, substitute_vst s visited t)
let substitute_vst s vst =
  try
    substitute_vst s Int_set.empty vst
  with CyclicSubstitutionInner -> raise @@ CyclicVSTSubst s

let rec substitute_var_in_vst
          (newvst: g_vs_typ) t (oldvst: g_vs_typ) : g_vs_typ =
  match oldvst with
  | VSTVertex -> VSTVertex
  | VSTVar t' when t = t' -> newvst
  | VSTVar _ -> oldvst
  | VSTUVar _ -> oldvst
  | VSTProd vsts -> VSTProd (List.map (substitute_var_in_vst newvst t) vsts)
  | VSTCoRec (t', vst) when t = t' -> oldvst
  | VSTCoRec (t', vst) -> VSTCoRec (t', substitute_var_in_vst newvst t vst)

let rec substitute_vs_in_gr (s : vs_subst) (gr : g_graph) : g_graph =
  transform_graph id (substitute_vs s) id gr

let substitute_vs_in_gr_subst (s : vs_subst) (gr_subst : gr_subst) : gr_subst =
  Subst.map (substitute_vs_in_gr s) gr_subst

let rec substitute_gr_in_ty (s : gr_subst) ty =
  transform_typ err (substitute_gr s) id id ty

let rec substitute_vs_in_ty (s : vs_subst) (ty : g_typ) : g_typ =
  transform_typ err (substitute_vs_in_gr s) (substitute_vs s) id ty

let rec substitute_vs_in_expr (s : vs_subst) (e : g_expr) : g_expr =
  transform_expr err id (substitute_vs_in_ty s) (substitute_vs_in_gr s) (substitute_vs s) id id e

let rec substitute_vst_in_vs (s : vst_subst) (vs : g_vertex_struct) : g_vertex_struct =
  transform_vertex_struct id (substitute_vst s) vs

let rec substitute_vst_in_vs_subst (s : vst_subst) (subst : vs_subst) : vs_subst =
  Subst.map (substitute_vst_in_vs s) subst

let rec substitute_vst_in_gr (s : vst_subst) (vs : g_graph) : g_graph =
  transform_graph id id (substitute_vst s) vs

let rec substitute_vst_in_expr (s : vst_subst) (e : g_expr) : g_expr =
  transform_expr err id id
    (substitute_vst_in_gr s)
    (substitute_vst_in_vs s)
    (fun (x, vst) -> (x, substitute_vst s vst))
    (substitute_vst s)
    e

let substitute_gr_env (s : gr_subst) (gr_ctx : gr_ctx) : gr_ctx =
  GrCtx.map (substitute_gr s) gr_ctx

let substitute_gr_in_expr (s : gr_subst) (e : g_expr) : g_expr =
  transform_expr err id (substitute_gr_in_ty s) (substitute_gr s) id id id e

let substitute_gr_vs_in_gr (s1 : gr_subst) (s2 : vs_subst) (g : g_graph) : g_graph =
  substitute_vs_in_gr s2 (substitute_gr s1 g)

let substitute_gr_vs_in_ty (s1 : gr_subst) (s2 : vs_subst) (t : g_typ) : g_typ =
  substitute_vs_in_ty s2 (substitute_gr_in_ty s1 t)

let substitute_gr_vs_in_expr (s1 : gr_subst) (s2 : vs_subst) (e : g_expr) : g_expr =
  substitute_vs_in_expr s2 (substitute_gr_in_expr s1 e)

let rec substitute_gr_in_decl (s : gr_subst) (d : g_decl) : g_decl =
  let rewrap d dd : g_decl =
    { ddesc = dd;
      dloc = d.dloc;
      dinfo = transform_schema (substitute_gr_in_ty s) d.dinfo
    }
  in
  match d.ddesc with
  | DVal (n, t, e) -> rewrap d (DVal (n, t, substitute_gr_in_expr s e))
  | DExp e -> rewrap d (DExp (substitute_gr_in_expr s e))
  | _ -> d

let rec substitute_vert_var e (x : vertex_var) replacement =
  match e with
  | VSVar y when x = y -> replacement
  | VSVar y -> VSVar y
  | VSUVar i -> VSUVar i
  | VSTuple es -> VSTuple (List.map (fun e -> substitute_vert_var e x replacement) es)
  | VSProj (e, i, n) -> VSProj (substitute_vert_var e x replacement, i, n)

let rec substitute_vert_var_in_gr gr (name : vertex_var) vs =
  let recur g = substitute_vert_var_in_gr g name vs in
  match gr with
  | GEmpty | GUVar _ | GVar _ -> gr
  | GSeq (g1, g2) -> GSeq (recur g1, recur g2)
  | GOr (g1, g2) -> GOr (recur g1, recur g2)
  | GPar (g1, g2) -> GPar (recur g1, recur g2)
  | GFut (g, vs') ->
     GFut (recur g, substitute_vert_var vs' name vs)
  | GTouch vs' -> GTouch (substitute_vert_var vs' name vs)
  | GPi (uf, _, ut, _, g) when uf = name || ut = name -> gr
  | GPi (uf, uft, ut, utt, g) ->
     GPi (uf, uft, ut, utt, recur g)
  | GRec (gv, g) -> GRec (gv, recur g)
  | GNew (u, _, g) when u = name -> gr
  | GNew (u, ut, g) -> GNew (u, ut, recur g)
  | GApp (g, uf, ut) ->
     GApp (recur g,
           substitute_vert_var uf name vs,
           substitute_vert_var ut name vs)

let substitute_vert_var_in_typ (ty : g_typ) (name : vertex_var) (vs : g_vertex_struct) : g_typ =
  transform_typ err
    (fun g -> substitute_vert_var_in_gr g name vs)
    (fun v -> substitute_vert_var v name vs)
    id
    ty

(** prioritizes s1 over s2. applies s1 to s2 *)
let overwrite_subst substitute (s1 : 'a Subst.t) (s2 : 'a Subst.t) : 'a Subst.t =
  let merge _ x y = match x, y with
    | Some x, _ -> Some x
    | None, Some y -> Some (substitute s1 y)
    | None, None -> None
  in Subst.merge merge s1 s2

let rec substitute_vst s (vst : g_vs_typ) : g_vs_typ =
  match vst with
  | VSTVertex | VSTVar _ -> vst
  | VSTUVar i -> (try Subst.find i s with Not_found -> vst)
  | VSTProd vs -> VSTProd (List.map (substitute_vst s) vs)
  | VSTCoRec (t, vst) -> VSTCoRec (t, substitute_vst s vst)

(** prioritizes s1 over s2. applies s1 to s2 *)
let overwrite_gr_subst = overwrite_subst substitute_gr
(** prioritizes s1 over s2. applies s1 to s2 *)
let overwrite_vs_subst = overwrite_subst substitute_vs
(** prioritizes s1 over s2. applies s1 to s2 *)
let overwrite_vst_subst = overwrite_subst substitute_vst

let with_subsituted_gr_ctx subst (m : 'a grchecker) : 'a grchecker =
  with_transformed_gr_ctx (substitute_gr_env subst) m
(* let with_subsituted_spawn_ctx subst (m : 'a grchecker) : 'a grchecker =
  with_transformed_spawn_ctx (substitute_vs_env subst) m
let with_subsituted_touch_ctx subst (m : 'a grchecker) : 'a grchecker =
  with_transformed_touch_ctx (substitute_vs_env subst) m *)


let rec free_vert_unif_vars_vs (vs : g_vertex_struct) =
  match vs with
  | VSVar _ -> []
  | VSUVar i -> [i]
  | VSTuple vss -> List.concat_map free_vert_unif_vars_vs vss
  | VSProj (vs, _, _) -> free_vert_unif_vars_vs vs

let rec free_vert_unif_vars_gr (gr : g_graph) =
  match gr with
  | GEmpty -> []
  | GVar _ -> []
  | GUVar _ -> []
  | GSeq (g1, g2)
  | GOr (g1, g2)
  | GPar (g1, g2) -> free_vert_unif_vars_gr g1 @ free_vert_unif_vars_gr g2
  | GFut (g, v) -> free_vert_unif_vars_gr g @ free_vert_unif_vars_vs v
  | GTouch v -> free_vert_unif_vars_vs v
  | GPi (_, _, _, _, g)
  | GRec (_, g)
  | GNew (_, _, g) -> free_vert_unif_vars_gr g
  (* Note: we don't add the touch parameters *)
  | GApp (g, v1, v2) -> free_vert_unif_vars_gr g  @ free_vert_unif_vars_vs v1
(* @ free_vert_unif_vars_vs v2 *)

let rec free_vert_unif_vars_ty (ty : g_typ) =
  match ty.tdesc with
  | TVar _ -> []
  | TUVar _ -> []
  | TRef ty -> free_vert_unif_vars_ty ty
  | TArrow (i, o, _, _, gr) -> free_vert_unif_vars_ty i @ free_vert_unif_vars_ty o @ free_vert_unif_vars_gr gr
  | TProd ts -> List.concat_map free_vert_unif_vars_ty ts
  | TFuture (v, t) -> free_vert_unif_vars_vs v @ free_vert_unif_vars_ty t
  | TConstr (_, ts, v) -> free_vert_unif_vars_vs v @ List.concat_map free_vert_unif_vars_ty ts
  | TRecType ts -> List.concat_map (fun (_, t) -> free_vert_unif_vars_ty t) ts

let rec free_spawn_vert_unif_vars_expr (e : g_expr) =
  let vars_edesc = match e.edesc with
    | EVar _ -> []
    | EConst (Futref vs) -> free_vert_unif_vars_vs vs
    | EConst _ -> []
    | ELet (_, _, e1, e2) -> free_spawn_vert_unif_vars_expr e1 @ free_spawn_vert_unif_vars_expr e2
    | EApp (e1, v1, v2, e2) ->
       free_vert_unif_vars_vs v1 @
         (* Note: we don't add the touch parameters *)
         (* free_vert_unif_vars_vs v2 @ *)
      free_spawn_vert_unif_vars_expr e1 @
      free_spawn_vert_unif_vars_expr e2
    | EFuture (vs, e) ->
      free_vert_unif_vars_vs vs @
      free_spawn_vert_unif_vars_expr e
    | EForce e -> free_spawn_vert_unif_vars_expr e
    | EPar (e1, e2) ->
      free_spawn_vert_unif_vars_expr e1 @
      free_spawn_vert_unif_vars_expr e2
    | ENewVert (_, _, e) -> free_spawn_vert_unif_vars_expr e
    | EFunc (_, _, _, _, _, _, _, e) -> free_spawn_vert_unif_vars_expr e
    | ETuple es -> List.concat_map free_spawn_vert_unif_vars_expr es
    | EInfixop (_, e1, e2) ->
      free_spawn_vert_unif_vars_expr e1 @
      free_spawn_vert_unif_vars_expr e2
    | EIf (e1, e2, e3) ->
      free_spawn_vert_unif_vars_expr e1 @
      free_spawn_vert_unif_vars_expr e2 @
      free_spawn_vert_unif_vars_expr e3
    | ELetTuple (_, e1, e2) ->
      free_spawn_vert_unif_vars_expr e1 @
      free_spawn_vert_unif_vars_expr e2
    | ELetRecord (_, e1, e2) ->
      free_spawn_vert_unif_vars_expr e1 @
      free_spawn_vert_unif_vars_expr e2
    | EMatch (scrutinee, cases) ->
      free_spawn_vert_unif_vars_expr scrutinee @
      List.concat_map (fun (_, _, e) -> free_spawn_vert_unif_vars_expr e) cases
    | ERef e -> free_spawn_vert_unif_vars_expr e
    | EDeref e -> free_spawn_vert_unif_vars_expr e
    | EUpdate (e1, e2) ->
      free_spawn_vert_unif_vars_expr e1 @
      free_spawn_vert_unif_vars_expr e2
    | ETry (e, cases) ->
      free_spawn_vert_unif_vars_expr e @
      List.concat_map (fun (_, _, e) -> free_spawn_vert_unif_vars_expr e) cases
    | EAnnot (e, _) -> free_spawn_vert_unif_vars_expr e
  in
  vars_edesc
  (*
  let vars_egr = free_vert_unif_vars_gr e.egr in
  let vars_etyp = free_vert_unif_vars_ty e.etyp in
  List.sort_uniq Int.compare (vars_edesc @ vars_egr @ vars_etyp)
   *)

let free_vert_unif_vars_ty_ctx (ty_ctx : ty_ctx) =
  let typs = List.map (fun (_, x) -> extract_ty_from_schema x) (TyCtx.bindings ty_ctx) in
  List.concat_map free_vert_unif_vars_ty typs

let collect u vertices =
  let* vsut_ctx = get_vs_unif_typ_ctx in

  let* vst_prod_ty_unif_idx = new_vs_typ_unif_var in
  (* message "vst_prod_ty_unif_idx";
     pprint Format.pp_print_int vst_prod_ty_unif_idx ; *)

  let* num_of_components =
    (try
       return @@ List.fold_left
                   (fun n i ->
                     match VSUTCtx.find i vsut_ctx with
                     | VSTProd [] -> n
                     | _ -> n + 1)
                   0
                   vertices
     with Not_found ->
       fail (GrErrUnknown
               ("unknown index when attempting to infer local vertices"))
    )
  in
  let* prod_tys, vert_idx_subst = (
      try
        return @@ List.fold_left (fun (tys, subst) i ->
                      match VSUTCtx.find i vsut_ctx with
                      | VSTProd [] ->
                         (tys,
                          Subst.add i
                            (VSTuple [])
                            subst)
                      | vst ->
                         ( tys @ [vst]
                         , Subst.add
                             i
                             (if num_of_components > 1 then
                                VSProj ((VSVar u), List.length tys, VSTUVar vst_prod_ty_unif_idx)
                              else
                                VSVar u)
                             subst
                         )
                    )
                    ([], Subst.empty)
                    vertices
      with Not_found -> fail (GrErrUnknown ("unknown index when attempting to infer local vertices"))
    ) in

  match prod_tys with
  | [x] -> return (x, vert_idx_subst)
  | _ ->
     let ty = VSTProd prod_tys in
     let s = Subst.singleton vst_prod_ty_unif_idx ty in
     return (ty,
             substitute_vst_in_vs_subst s vert_idx_subst)

let g_typ_to_t_typ t =
  transform_typ (fun a -> 0)
    (fun _ -> ())
    (fun _ -> ())
    (fun _ -> ())
    t

let rec vtot (vst_ctx: g_vs_typ TyCtx.t)
          (next_vst_var: int)
          (ty: t_typ) =
  message "vtot:";
  pprint pprint_t_typ ty;
  let* tyname_ctx = get_tyname_ctx in
  let prod vsts =
    VSTProd
      (List.fold_left
         (fun vsts vst ->
           match vst with
           | VSTProd l -> vsts @ l
           | _ -> vsts @ [vst]
         )
         []
             vsts)
  in
  match ty.tdesc with
  | TVar a ->
     (try
        return @@ TyCtx.find a vst_ctx
          with Not_found -> return @@ VSTProd []
        )
      | TUVar _ -> fail GrErrResidualTyUnifVar
      | TRef t -> vtot vst_ctx next_vst_var t
      | TArrow _ -> return @@ VSTProd []
      | TProd ts ->
        let* vts = mapM (vtot vst_ctx next_vst_var) ts
        in return @@ prod vts
      | TFuture (_, t) ->
        let* vst = vtot vst_ctx next_vst_var t in
        return @@ prod [vst; VSTVertex]
      (* | TConstr (tid, [], _) -> return @@ VSTProd [] *)
      | TConstr (tid, args, _) ->
         (try return @@ TyCtx.find (string_of_longid tid) vst_ctx
          with Not_found ->
            (try
               let (_, s) = TyCtx.find (string_of_longid tid) tyname_ctx in
              let* g_args = mapM (vtot vst_ctx next_vst_var) args in
              let var = "t" ^ (string_of_int next_vst_var) in
              let vst_ctx =
                TyCtx.add (string_of_longid tid) (VSTVar var) vst_ctx
              in
              let* vst = vtos vst_ctx (next_vst_var + 1) g_args s in
              return @@ VSTCoRec (var, vst)
             with Not_found ->
             (* Builtin type constructors like "int" currently trip this
              * case because we don't add them to the context.
              * Presumably we shouldn't hit this case otherwise since
              * typechecking worked. So maybe it's fine to just return
              * [] here? *)
               return @@ VSTProd []
            (* XXX fail (GrErrUnboundTypeCon (string_of_longid tid)) *)
            )
        )
      | TRecType constructors ->
        let* vsts =
          mapM (fun (_, t) -> vtot vst_ctx next_vst_var t) constructors
        in
        return @@ prod vsts

and vtos (vst_ctx: g_vs_typ TyCtx.t)
(next_vst_var: int)
(args: g_vs_typ list)
(s: g_schema) =
  match (s, args) with
  | (SMono t, args) -> vtot vst_ctx next_vst_var (g_typ_to_t_typ t)
  | (SForall (a, s), vst::args) ->
     vtos (TyCtx.add a vst vst_ctx) next_vst_var args s
  | (SForallG (_, s), args) ->
     vtos vst_ctx next_vst_var args s
  | _ -> fail (GrErrUnknown "mismatch in number of type arguments")

and vs_ty_of_typ (ty: t_typ) : g_vs_typ grchecker =
  inspect (TType ty) begin
      let* tyname_ctx = get_tyname_ctx in
      let* result = vtot InferTy.Ctx.empty 1 ty in
      match result with
      | VSTCoRec (t, VSTVar t') when t = t' -> return @@ VSTProd []
      | VSTCoRec (t, VSTProd [VSTVar t']) when t = t' -> return @@ VSTProd []
      | _ -> return result
    end

let rec t_typ_to_g_typ ?(unit_spawn_params = false) (ty : t_typ) : g_typ grchecker =
  let recur = t_typ_to_g_typ ~unit_spawn_params in
  inspect (TType ty) begin
    let rebuild tdesc = { ty with tdesc } in
    match ty.tdesc with
      | TVar x -> return @@ rebuild (TVar x)
      | TUVar _ -> fail GrErrResidualTyUnifVar
      | TRef t ->
        let* t_annot = (recur t) in
        return @@ rebuild (TRef t_annot)
      | TArrow (t1, t2, _, _, _) ->
        if unit_spawn_params then
          let* t1_annot = t_typ_to_g_typ ~unit_spawn_params:true t1 in
          let* t2_annot = t_typ_to_g_typ ~unit_spawn_params:true t2 in
          let* gr_unif_idx = new_graph_unif_var in
          let* touch_vert = new_vertex_name in
          let* spawn_vert = new_vertex_name in
          let* vsut_ctx = get_vs_unif_typ_ctx in
          let touch_params = free_vert_unif_vars_ty t1_annot in
          let* (prod_ty, vert_idx_subst) = collect touch_vert touch_params
          in
          let t1_annot = substitute_vs_in_ty vert_idx_subst t1_annot in
          return @@ rebuild (TArrow (
            t1_annot, t2_annot,
            (spawn_vert, VSTProd []),
            (touch_vert, prod_ty),
            (GApp (GUVar gr_unif_idx, VSTuple [], VSVar touch_vert))
          ))
        else
          let* t1_annot = t_typ_to_g_typ ~unit_spawn_params:true t1 in
          let* t2_annot = t_typ_to_g_typ ~unit_spawn_params:true t2 in
          let* gr_unif_idx = new_graph_unif_var in
          let* touch_vert = new_vertex_name in
          let* spawn_vert = new_vertex_name in
          let* vsut_ctx = get_vs_unif_typ_ctx in
          let touch_params = free_vert_unif_vars_ty t1_annot in
          let* (prod_ty, vert_idx_subst) = collect touch_vert touch_params
          in
          let spawn_params = free_vert_unif_vars_ty t2_annot in
          let* (spawn_prod_ty, spawn_vert_idx_subst) =
            collect spawn_vert spawn_params
          in
          let t1_annot = substitute_vs_in_ty vert_idx_subst t1_annot in
          let t2_annot = substitute_vs_in_ty spawn_vert_idx_subst t2_annot in

          return @@ rebuild (TArrow (
            t1_annot, t2_annot,
            (spawn_vert, spawn_prod_ty),
            (touch_vert, prod_ty),
            (GApp (GUVar gr_unif_idx, VSVar spawn_vert, VSVar touch_vert))
          ))
      | TProd ts ->
        let* ts_annot = mapM recur ts in
        return @@ rebuild (TProd ts_annot)
      | TFuture (_, t) ->
        let* t_annot = recur t in
        let* vs = new_vertex_unif_var VSTVertex in
        return @@ rebuild (TFuture (VSUVar vs, t_annot))
      | TConstr (tid, args, _) ->
        let* args_annot = mapM recur args in
        let* vst = vs_ty_of_typ ty in
        message "t_typ_to_g_typ ty:";
        pprint pprint_t_typ ty;
        message "vst:";
        pprint pprint_g_vs_typ vst;
        let* vs = new_vertex_unif_var vst in
        return @@ rebuild (TConstr (tid, args_annot, VSUVar vs))
      | TRecType constructors ->
        let* cons_annot = mapM
                            (fun (id, t) ->
                              let* t_annot = recur t in
                              return (id, t_annot))
                            constructors
     in
     return @@ rebuild (TRecType cons_annot)
  end

let add_rec_ty_to_ctx
          name
          (tyvars: string list)
          (constructors: (string * t_typ) list)
          ((tyname_ctx : tyname_ctx), con_ctx, tyctx)
  =
  let typ tdesc = { tdesc = tdesc; tloc = dloc } in
  let var = "t0" in
  let vst_ctx =
    TyCtx.add name (VSTVar var) TyCtx.empty
  in
  let* vsty = vtot vst_ctx 1 (typ @@ TRecType constructors)
  in
  let vsty = VSTCoRec(var, vsty) in
  (*let* unif_var = new_vertex_unif_var vsty in *)
  let* vname = new_vertex_name in
  pprint (fun f () -> Format.fprintf f "VS type for %s: %a"
                     name
                     pprint_g_vs_typ vsty
    )
  ();
  let ty_alias =
    typ @@ TConstr (Id name,
                    List.map (fun x -> typ @@ TVar x) tyvars,
                    VSVar vname)
  in
  let mk_schema t =
    List.fold_left (fun t a -> SForall (a, t)) (SMono t) tyvars
  in
  let* uign = new_vertex_name in
  if not (List.exists (fun (_, t) -> InferTy.contains_future t) constructors)
  then
    let* constructors =
      mapM (fun (id, t) ->
          let* t = t_typ_to_g_typ t in
          return (id, t))
        constructors
    in
    let add_const con_ctx (conid, argty) =
      let t = typ (TArrow (argty,
                           ty_alias,
                           (uign, VSTProd []),
                           (vname, VSTProd []),
                           GEmpty))
      in
      let s = mk_schema t in
      pprint (fun f () -> Format.fprintf f "%s: %a" conid pprint_g_schema s) ();
      TyCtx.add conid s con_ctx
    in
    return
    (TyCtx.add name (tyvars, mk_schema (typ (TRecType constructors))) tyname_ctx,
     List.fold_left add_const con_ctx constructors,
     List.fold_left add_const tyctx constructors)
  else
  let rec traverse_typ
            (ty: t_typ)
            (vs: g_vertex_struct)
            (offset: int) (* how many projections used up to now *)
            (vsts: g_vs_typ list) (* types of projections up to now *)
          : (g_typ * int (* how many projections used after *)
             * g_vs_typ list (* types of projections after *)
            ) grchecker =
  match ty.tdesc with
    | TVar v ->
       return (typ @@ TVar v, offset, vsts)
    | TUVar _ -> fail GrErrResidualTyUnifVar
    | TRef t -> traverse_typ t vs offset vsts
    | TArrow _ -> let* t = t_typ_to_g_typ ty in
                  return (t, offset, vsts)
    | TProd ts ->
       let* (ts, offset, vsts) =
         foldM
           (fun (ts, offset, vsts) t ->
             let* (t, offset', vsts') = traverse_typ t vs offset vsts in
             return (ts @ [t], offset', vsts'))
           ([], offset, vsts)
           ts
       in return (typ @@ TProd ts, offset, vsts)
    | TFuture (_, t) ->
       let* (gt, offset', vsts') = traverse_typ t vs offset vsts in
       (* We'll fill in the real type later *)
       return (typ @@ TFuture (VSProj (vs, offset', VSTProd []), gt),
               offset' + 1,
               VSTVertex::vsts'
         )
    | TConstr (name', args, _) ->
       let* (ts, offset, vsts) =
         foldM
           (fun (ts, offset, vsts) t ->
             let* (t, offset', vsts') = traverse_typ t vs offset vsts in
             return (ts @ [t], offset', vsts'))
           ([], offset, vsts)
           args
       in
       let* vst = if name = string_of_longid name'
                  then return vsty else vs_ty_of_typ ty
       in
       let (vs, offset', vsts') =
         match vst with
         | VSTProd [] -> (VSTuple [], offset, vsts)
         | _ -> (VSProj (vs, offset, VSTProd []), offset + 1, vst::vsts)
       in
       return (typ @@ TConstr (name', ts, vs),
               offset',
               vsts')
    | TRecType _ -> unimplemented "traverse_typ TRecType"
  in
  let redo_types vst ty =
    transform_typ
      (fun x -> x)
      (fun x -> x)
      (function VSProj (vs, i, _) -> VSProj (vs, i, vst)
              | x -> x)
      (fun x -> x)
      ty
  in
  let* x, _, vsts = foldM
    (fun (constructors, offset, vsts) (id, t) ->
      let* (t, offset', vsts') = traverse_typ t (VSVar vname) offset vsts
      in
      return (constructors @ [(id, t)], offset', vsts'))
    ([], 0, [])
    constructors
  in
  let vsts = List.rev vsts in
    let ty_alias =
    typ @@ TConstr (Id name, List.map (fun x -> typ @@ TVar x) tyvars,
                    VSVar vname)
  in
  let* constructors =
    mapM (fun (id, t) -> return (id, redo_types (VSTProd vsts) t)) x
  in
  let add_const con_ctx (conid, argty) =
    let t = typ (TArrow (argty,
                         ty_alias,
                         (uign, VSTProd []),
                         (vname, vsty),
                         GEmpty))
    in
    let s = mk_schema t in
    pprint (fun f () -> Format.fprintf f "%s: %a" conid pprint_g_schema s) ();
    TyCtx.add conid s con_ctx

  in
  return
    (TyCtx.add name (tyvars, mk_schema (typ (TRecType constructors))) tyname_ctx,
     List.fold_left add_const con_ctx constructors,
     List.fold_left add_const tyctx constructors)

let rec contains_unif_var_vs i (v : g_vertex_struct) : bool =
  match v with
  | VSVar _ -> false
  | VSUVar j -> i = j
  | VSTuple vs -> List.fold_left (fun sum v -> sum || contains_unif_var_vs i v) false vs
  | VSProj (v, _, _) -> contains_unif_var_vs i v

let rec contains_unif_var_vst i (v : g_vs_typ) : bool =
  match v with
  | VSTVertex -> false
  | VSTVar _ -> false
  | VSTUVar j -> i = j
  | VSTProd vs -> List.fold_left (fun sum v -> sum || contains_unif_var_vst i v) false vs
  | VSTCoRec (_, v) -> contains_unif_var_vst i v

let rec contains_unif_var_gr i (g : g_graph) : bool =
  match g with
  | GEmpty
  | GVar _ -> false
  | GUVar j -> i = j
  | GSeq (g1, g2)
  | GOr (g1, g2)
  | GPar (g1, g2) -> contains_unif_var_gr i g1 || contains_unif_var_gr i g2
  | GFut (g, v) -> contains_unif_var_gr i g
  | GTouch v -> false
  | GPi (_, _, _, _, g)
  | GRec (_, g)
  | GNew (_, _, g)
  | GApp (g, _, _) -> contains_unif_var_gr i g

let rec rewriting_rules_gr g = match g with
| GSeq (GEmpty, x) -> x
| GSeq (x, GEmpty) -> x
| GApp (GEmpty, _, _) -> GEmpty
(* | GOr (_, _) -> _
| GPar (_, _) -> _
| GFut (_, _) -> _
| GTouch _ -> _
| GPi (_, _, _, _, _) -> _
| GRec (_, _) -> _
| GNew (_, _, _) -> _
| GApp (_, _, _) -> _ *)
| _ -> g

let rec rewrite_gr g = match g with
| GEmpty
| GVar _
| GUVar _ -> g
| GSeq (g1, g2) -> rewriting_rules_gr (GSeq (rewrite_gr g1, rewrite_gr g2))
| GOr (g1, g2) -> rewriting_rules_gr (GOr (rewrite_gr g1, rewrite_gr g2))
| GPar (g1, g2) -> rewriting_rules_gr (GPar (rewrite_gr g1, rewrite_gr g2))
| GFut (g, v) -> rewriting_rules_gr (GFut (rewrite_gr g, v))
| GTouch v -> GTouch v
| GPi (x, xt, y, yt, g) -> rewriting_rules_gr (GPi (x, xt, y, yt, rewrite_gr g))
| GRec (x, g) -> rewriting_rules_gr (GRec (x, rewrite_gr g))
| GNew (x, xt, g) -> rewriting_rules_gr (GNew (x, xt, rewrite_gr g))
| GApp (g, x, y) -> rewriting_rules_gr (GApp (rewrite_gr g, x, y))

let collect_gr_or grs = match grs with
| [] -> GEmpty
| gr :: [] -> gr
| gr :: grs -> List.fold_left (fun sum gr -> GOr (sum, gr)) gr grs

let rec build_seq xs = match xs with
| [] -> GEmpty
| x :: [] -> x
| x :: xs -> GSeq (x, build_seq xs)

let rec free_vertex_vars_in_vs vs =
  match vs with
  | VSVar v -> [v]
  | VSUVar _ -> []
  | VSTuple vss -> List.concat (List.map free_vertex_vars_in_vs vss)
  | VSProj (vs, _, _) -> free_vertex_vars_in_vs vs

let vertex_var_eq (VId a) (VId b) =
  UnionFind.findkey a = UnionFind.findkey b

let rec free_vertex_vars_in_gr g =
  let veq = vertex_var_eq in
  match g with
  | GEmpty | GVar _ | GUVar _ -> []
  | GSeq (g1, g2)
    | GOr (g1, g2)
    | GPar (g1, g2) ->
     (free_vertex_vars_in_gr g1) @ (free_vertex_vars_in_gr g2)
  | GFut (g, vs) -> (free_vertex_vars_in_gr g) @ (free_vertex_vars_in_vs vs)
  | GTouch vs -> free_vertex_vars_in_vs vs
  | GPi (uf, _, ut, _, g) ->
     List.filter
       (fun u -> (not (veq u uf)) && (not (veq u ut)))
       (free_vertex_vars_in_gr g)
  | GRec (_, g) -> free_vertex_vars_in_gr g
  | GNew (u, _, g) ->
     List.filter
       (fun u' -> (not (veq u' u)))
       (free_vertex_vars_in_gr g)
  | GApp (g, uf, ut) ->
     (free_vertex_vars_in_gr g)
     @ (free_vertex_vars_in_vs uf)
     @ (free_vertex_vars_in_vs ut)
