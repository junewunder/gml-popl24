open Ast
open GrUtil
open AstPrint
open GrPrint
open Visitor

(* in this file: "gr" is used to mean "graph" and "vs" is used to mean "vertex structure" *)

(* let unify_substs unify overwrite s1 s2 =
  let unifs1 = List.map fst (Subst.bindings s1) in
  let unifs2 = List.map fst (Subst.bindings s2) in
  let unifs = List.sort_uniq Int.compare (unifs1 @ unifs2) in
  let merge i =
    match Subst.find_opt i s1, Subst.find_opt i s2 with
    | Some x, Some y -> unify x y
    | Some t, None | None, Some t -> return @@ Subst.singleton i t
    | None, None -> fail (GrErrUnknown "128746894250397")
  in
  let* substs = all (List.map merge unifs) in
  return @@ List.fold_left overwrite Subst.empty substs *)

let idM = (fun x -> return x)

let rec transform_vs_typ_monadic
          (f1 : 'g_unif1 -> 'g_unif2)
          (vst: 'g_unif vs_typ) =
  match vst with
  | VSTVertex -> return VSTVertex
  | VSTVar t -> return @@ VSTVar t
  | VSTUVar i -> let* i = f1 i in return @@ VSTUVar i
  | VSTProd vsts ->
     let* vsts = mapM (transform_vs_typ_monadic f1) vsts in
     return @@ VSTProd vsts
  | VSTCoRec (t, vst) ->
     let* vst = transform_vs_typ_monadic f1 vst in
     return @@ VSTCoRec (t, vst)

let rec transform_vertex_struct_monadic
          transform_unif_var
          transform_vst
          vs
  =
  let recur =
    transform_vertex_struct_monadic transform_unif_var transform_vst
  in
  match vs with
  | VSVar x -> return @@ VSVar x
  | VSUVar x ->
     let* x = transform_unif_var x in
     return @@ VSUVar x
  | VSTuple x ->
     let* xs = mapM recur x in
     return @@ VSTuple xs
  | VSProj (x, y, n) ->
     let* x = recur x in
     let* n = transform_vst n in
     return @@ VSProj (x, y, n)

let rec transform_typ_monadic
          transform_unif_var
          transform_gr
          transform_vs
          transform_vert_param
          transform_vs_typ
          t =
  inspect (GType t) begin
  let recur = transform_typ_monadic
          transform_unif_var
          transform_gr
          transform_vs
          transform_vert_param
          transform_vs_typ
  in
  let* tdesc = match t.tdesc with
    | TVar x -> return @@ TVar x
    | TUVar i ->
      let* i = transform_unif_var i in
      return @@ TUVar i
    | TRef t ->
      let* t = recur t in
      return @@ TRef t
    | TArrow (i, o, v_s, v_t, g) ->
      let* i = recur i in
      let* o = recur o in
      let* v_s = transform_vert_param v_s in
      let* v_t = transform_vert_param v_t in
      let* g = transform_gr g in
      return (TArrow (i, o, v_s, v_t, g))
    | TProd ts ->
      let* ts = mapM recur ts in
      return @@ TProd ts
    | TFuture (v, t) ->
      let* t = recur t in
      let* v = transform_vs v in
      return @@ TFuture (v, t)
    | TConstr (x, ts, v) ->
      let* ts = mapM recur ts in
      let* v = transform_vs v in
      return @@ TConstr (x, ts, v)
    | TRecType xs ->
      let* xs = mapM (fun (x, t) -> let* t = recur t in return (x, t)) xs in
      return @@ TRecType xs
  in return {t with tdesc}
    end

let rec transform_schema_monadic
  (f1 : ('t_unif1, 'graph1, 'vert1, 'vert_param1) typ -> ('t_unif2, 'graph2, 'vert2, 'vert_param2) typ grchecker)
  (sch : ('t_unif1, 'graph1, 'vert1, 'vert_param1) schema) =
  let recur = transform_schema_monadic f1 in
  match sch with
  | SMono t ->
    let* t = f1 t in
    return @@ SMono t
  | SForall (x, sch) ->
    let* sch = recur sch in
    return @@ SForall (x, sch)
  | SForallG (x, sch) ->
    let* sch = recur sch in
    return @@ SForallG (x, sch)

let rec inst (t : g_schema) : g_typ grchecker =
  let rec inst_gr (x : string) (i : int) (gr : g_graph) = match gr with
    | GVar y when x = y -> GUVar i
    | GEmpty | GVar _ | GUVar _ | GTouch _ -> gr
    | GSeq (l, r) -> GSeq (inst_gr x i l, inst_gr x i r)
    | GOr (l, r) -> GOr (inst_gr x i l, inst_gr x i r)
    | GPar (l, r) -> GPar (inst_gr x i l, inst_gr x i r)
    | GFut (g, v) -> GFut (inst_gr x i g, v)
    | GPi (uf, uft, ut, utt, g) -> GPi (uf, uft, ut, utt, inst_gr x i g)
    | GRec (s, _) when s = x -> gr
    | GRec (s, g) -> GRec (s, inst_gr x i g)
    | GNew (a, b, g) -> GNew (a, b, inst_gr x i g)
    | GApp (g, a, b) -> GApp (inst_gr x i g, a, b)
  in
  let rec inst_ty (x : string) (i : int) (t : g_typ) =
    transform_typ id (inst_gr x i) id id t
  in match t with
  | SMono t_inner -> return t_inner
  | SForall (x, t) -> inst t
  | SForallG (x, t) ->
    let* unif_idx = new_graph_unif_var in
    let* t' = inst t in
    return @@ inst_ty x unif_idx t'

let rec inst_graph (t : g_schema) : g_schema grchecker =
  let rec inst_gr (x : string) (i : int) (gr : g_graph) = match gr with
    | GVar y when x = y -> GUVar i
    | GEmpty | GVar _ | GUVar _ | GTouch _ -> gr
    | GSeq (l, r) -> GSeq (inst_gr x i l, inst_gr x i r)
    | GOr (l, r) -> GOr (inst_gr x i l, inst_gr x i r)
    | GPar (l, r) -> GPar (inst_gr x i l, inst_gr x i r)
    | GFut (g, v) -> GFut (inst_gr x i g, v)
    | GPi (uf, uft, ut, utt, g) -> GPi (uf, uft, ut, utt, inst_gr x i g)
    | GRec (s, _) when s = x -> gr
    | GRec (s, g) -> GRec (s, inst_gr x i g)
    | GNew (a, b, g) -> GNew (a, b, inst_gr x i g)
    | GApp (g, a, b) -> GApp (inst_gr x i g, a, b)
  in
  let rec inst_ty (x : string) (i : int) (t : g_typ) =
    transform_typ id (inst_gr x i) id id t
  in
  let rec inst_schema (x: string) (i: int) (s: g_schema) =
    match s with
    | SMono t -> SMono (inst_ty x i t)
    | SForall (y, s) -> SForall (y, inst_schema x i s)
    | SForallG (g, s) -> SForallG (g, inst_schema x i s)
  in match t with
  | SMono t_inner -> return t
  | SForall (x, t) -> let* t = inst_graph t in
                      return @@ SForall (x, t)
  | SForallG (x, t) ->
    let* unif_idx = new_graph_unif_var in
    let* t' = inst_graph t in
    pprint (fun f _ -> Format.fprintf f "Replace %s with %d\n" x unif_idx) ();
    return @@ inst_schema x unif_idx t'

let generalize (t : g_typ) : (g_schema * gr_subst) grchecker =
  let rec extract_unif_vars_gr (g : g_graph) : int list = match g with
    | GEmpty -> []
    | GVar _ -> []
    | GUVar i -> [i]
    | GSeq (l, r)
    | GOr (l, r)
    | GPar (l, r) -> (extract_unif_vars_gr l) @ (extract_unif_vars_gr r)
    | GTouch _ -> []
    | GFut (g, _)
    | GPi (_, _, _, _, g)
    | GRec (_, g)
    | GNew (_, _, g)
    | GApp (g, _, _) -> extract_unif_vars_gr g
  in
  let rec extract_unif_vars_ty (t : g_typ) : int list = match t.tdesc with
    | TVar _
    | TUVar _ -> []
    | TRef t -> extract_unif_vars_ty t
    | TArrow (i, o, _, _, g) -> extract_unif_vars_ty i @ extract_unif_vars_ty o @ extract_unif_vars_gr g
    | TProd ts -> List.concat_map extract_unif_vars_ty ts
    | TFuture (_, t) -> extract_unif_vars_ty t
    | TConstr (_, ts, _) -> List.concat_map extract_unif_vars_ty ts
    | TRecType xs -> List.concat_map (fun (x, t) -> extract_unif_vars_ty t) xs
  in
  let extract_gr_ctx_unif_vars gr_ctx : int list =
    let ctx_unif_vars = GrCtx.map extract_unif_vars_gr gr_ctx in
    (GrCtx.bindings ctx_unif_vars) |> List.map snd |> List.concat |> List.sort_uniq Int.compare
  in
  let build_ty_schema (t : g_typ) (var_names : string list) : g_schema =
    List.fold_right (fun i t -> SForallG (i, t)) var_names (SMono t)
  in
  let var_name_of_int i =
    "'g" ^ String.make 1 @@ Char.chr (97 + i)
  in
  let* gr_ctx = get_gr_ctx in
  let free_unif_ctx_vars = extract_gr_ctx_unif_vars gr_ctx in
  let unif_vars = uniq (extract_unif_vars_ty t) in
  let unif_vars = diff unif_vars free_unif_ctx_vars in
  let unif_vars_from_zero = List.init (List.length unif_vars) (fun x -> x) in
  let var_names = List.map var_name_of_int unif_vars_from_zero in
  let vars = List.map (fun x -> GVar x) var_names in
  let new_unif_var_names = (Subst.of_seq @@ List.to_seq (List.combine unif_vars vars)) in
  let generalized_ty = substitute_gr_in_ty new_unif_var_names t in
  message "generalize";
  pprint pprint_g_typ t;
  pprint pprint_g_typ generalized_ty;
  message "/generalize";
  return (build_ty_schema generalized_ty var_names, new_unif_var_names)

let rec unify_substs unify substitute fail_cannot_unify equal s1 s2 =
  let recur = unify_substs unify substitute fail_cannot_unify equal in
  let unifs1 = List.map fst (Subst.bindings s1) in
  let unifs2 = List.map fst (Subst.bindings s2) in
  let unifs = List.sort_uniq Int.compare (unifs1 @ unifs2) in

  let get_entry i =
    match Subst.find_opt i s1, Subst.find_opt i s2 with
    | Some x, Some y ->
      let* s = unify x y in
      let x = substitute s x in
      let y = substitute s y in
      if not (equal x y) then fail_cannot_unify x y else
      return (x, s)
    | Some x, None
    | None, Some x -> return (x, Subst.empty)
    | None, None -> fail (GrErrUnknown "unify_substs (None, None) case")
  in
  let f sum i =
    let* (x, s) = (get_entry i) in
    if not (Subst.is_empty s)
      then recur (Subst.add i x sum) s
      else return (Subst.add i x sum)
  in
  foldM f Subst.empty unifs

let rec unify_vs_types_int (id_pairs: (string * string) list)
          (vst1: g_vs_typ) (vst2: g_vs_typ) : vst_subst grchecker =
  let fail_cannot_unify = fail @@ GrErrCannotUnifyVSTs (vst1, vst2) in
  match vst1, vst2 with
  | VSTVertex, VSTVertex -> return Subst.empty
  | VSTVar x, VSTVar y when x = y -> return Subst.empty
  | VSTVar x, VSTVar y ->
     if List.assoc x id_pairs = y then return Subst.empty
     else fail_cannot_unify
  | VSTUVar i1, VSTUVar i2 when i1 = i2 -> return Subst.empty
  | VSTUVar i, x
    | x, VSTUVar i ->
     if contains_unif_var_vst i x then fail_cannot_unify
     else return @@ Subst.singleton i x
  | VSTProd [x], _ -> unify_vs_types_int id_pairs x vst2
  | _, VSTProd [x] -> unify_vs_types_int id_pairs vst1 x
  | VSTProd xs, VSTProd ys ->
     let* pairs = try return @@ List.combine xs ys
                  with Invalid_argument _ -> fail_cannot_unify
     in
     let f s1 (v1, v2) =
       let* s2 = unify_vs_types_int id_pairs v1 v2 in
       unify_vst_substs s1 s2
     in
     foldM f Subst.empty pairs
  | VSTCoRec (id1, vst1) , VSTCoRec (id2, vst2) ->
     unify_vs_types_int ((id1, id2)::id_pairs) vst1 vst2
  | VSTCoRec (id, vst), _ ->
     unify_vs_types_int id_pairs (substitute_var_in_vst vst1 id vst) vst2
  | _, VSTCoRec (id, vst)->
     unify_vs_types_int id_pairs vst1 (substitute_var_in_vst vst2 id vst)
  | _ -> fail_cannot_unify

  and unify_vs_types vst1 vst2 = unify_vs_types_int [] vst1 vst2

  and unify_vst_substs (s1 : vst_subst) (s2 : vst_subst) : vst_subst grchecker =
      let fail_cannot_unify v1 v2 = fail @@ GrErrCannotUnifyVSTs (v1, v2) in
      unify_substs unify_vs_types substitute_vst fail_cannot_unify equal_g_vs_typ s1 s2


let [@warning "-8"] rec unify_vs
          (v1 : g_vertex_struct) (v2 : g_vertex_struct) : vs_subst grchecker =
  inspect (UnifyVSs (v1, v2)) begin


      message "unify_vs: v1:";
      pprint pprint_g_vs v1;

      message "v2:";
      pprint pprint_g_vs v2;
    let fail_cannot_unify = fail @@ GrErrCannotUnifyVSs (v1, v2) in
    match v1, v2 with
    | VSVar x, VSVar y when x = y -> return Subst.empty
    | VSTuple [], _   (* these cases seem weird but should be safe, *)
      | _, VSTuple [] (* since there's only one VS of typ VSTProd [] *)
      -> return Subst.empty
    | VSVar (VId x), VSVar (VId y) ->
       UnionFind.union x y;
       return Subst.empty
    | VSUVar i1, VSUVar i2 when i1 = i2 -> return Subst.empty
    | VSProj (v1, 0, VSTProd [_]), v2 -> unify_vs v1 v2
    | v1, VSProj (v2, 0, VSTProd []) -> unify_vs v1 v2
    | VSProj (VSTuple vs, i, _), v2 -> unify_vs (List.nth vs i) v2
    | v1, VSProj (VSTuple vs, i, _) -> unify_vs v1 (List.nth vs i)
    | VSUVar i, x
    | x, VSUVar i ->
      if contains_unif_var_vs i x then fail_cannot_unify
      else return @@ Subst.singleton i x
    | VSTuple xs, VSTuple ys ->
      let* pairs = try return @@ List.combine xs ys
                   with Invalid_argument _ -> fail_cannot_unify
      in
      let f s1 (v1, v2) =
       let* s2 = unify_vs (substitute_vs s1 v1) (substitute_vs s1 v2) in
       unify_vs_substs s1 s2
      in
      foldM f Subst.empty pairs
    | VSProj(v1t, i, vst1), VSProj(v2t, j, vst2) ->
       unify_proj_with v1t i vst1 v2
      (*
       let* s = get_state in
       (match unify_proj_with v1t i vst1 v2 s with
        | Ok r -> return r
        | _ -> unify_proj_with v2t j vst2 v1
       )
       *)
    | VSProj(v1, i, vst1), _ -> unify_proj_with v1 i vst1 v2
    | _, VSProj(v2, i, vst2) -> unify_proj_with v2 i vst2 v1
    | _ -> fail_cannot_unify
  end
and unify_proj_with (v1: g_vertex_struct) (i: int) (vst: g_vs_typ) v2 =
  message "vst1:";
  pprint pprint_g_vs_typ vst;
  let* vsts1 =
    (match vst with
     | VSTProd vsts -> return vsts
     | VSTCoRec (t, VSTProd vsts) ->
        let [@warning "-8"] VSTProd vsts =
          substitute_var_in_vst vst t (VSTProd vsts)
        in
        return vsts
     | _ -> fail @@ GrErrCannotUnifyVSs (VSProj (v1, i, vst), v2)
        )
  in
  message "vsts:";
  pprint pprint_g_vs_typ (VSTProd vsts1);
  let* new_vars1 = foldM
                     (fun vars t ->
                       let* new_unif_var = new_vertex_unif_var t in
                          return ((VSUVar new_unif_var)::vars)
                     )
                     []
                     vsts1
  in
  let new_vars1 = List.rev new_vars1 in
  let* subst1 = unify_vs v1 (VSTuple new_vars1) in
  let* subst2 = unify_vs (List.nth new_vars1 i) (substitute_vs subst1 v2) in
  let* subst = unify_vs_substs subst1 subst2 in
  return subst

and unify_vs_substs (s1 : vs_subst) (s2 : vs_subst) : vs_subst grchecker =
  let fail_cannot_unify v1 v2 = fail @@ GrErrCannotUnifyVSs (v1, v2) in
  unify_substs unify_vs substitute_vs fail_cannot_unify equal_g_vertex_struct s1 s2

and unify_vs_substs_many (ss : vs_subst list) : vs_subst grchecker =
  foldM unify_vs_substs Subst.empty ss

let rec unify_gr_internal
          (gvar_pairs: (string * string) list)
          (g1 : g_graph) (g2 : g_graph) : gr_subst grchecker =
  inspect (UnifyGrs (g1, g2)) begin
    let fail_cannot_unify = fail @@ GrErrCannotUnifyGrs (g1, g2) in
    match g1, g2 with
    | GEmpty, GEmpty -> return Subst.empty
    | GVar x, GVar y ->
      if x = y || List.assoc x gvar_pairs = y then return Subst.empty
      else fail @@ GrErrCannotUnifyGrs (g1, g2)
    | GUVar i1, GUVar i2 when i1 = i2 -> return Subst.empty
    | GUVar i, x
    | x, GUVar i ->
      if contains_unif_var_gr i x then fail_cannot_unify
      else return (Subst.singleton i x)
    | GOr (g11, g12), GOr (g21, g22)
    | GSeq (g11, g12), GSeq (g21, g22)
    | GPar (g11, g12), GPar (g21, g22) ->
      let* s1 = unify_gr_internal gvar_pairs g11 g21 in
      let* s2 = unify_gr_internal gvar_pairs g12 g22 in
      unify_gr_substs s1 s2
    | GPi (x1, xt1, y1, yt1, g1), GPi (x2, xt2, y2, yt2, g2) ->
      (
        message "xt1, xt2";
        pprint pprint_vertex_var x1;
        pprint pprint_vertex_var x2;
        pprint pprint_g_vs_typ xt1;
        pprint pprint_g_vs_typ xt2
        );
      unify_vs_types xt1 xt2 >>
      return (
        message "yt1, yt2";
        pprint pprint_vertex_var y1;
        pprint pprint_vertex_var y2;
        pprint pprint_g_vs_typ yt1;
        pprint pprint_g_vs_typ yt2
        ) >>
      unify_vs_types yt1 yt2 >>
      unify_gr_internal gvar_pairs g1 g2
    | GRec (x1, g1), GRec (x2, g2) ->
       unify_gr_internal ((x1, x2)::gvar_pairs) g1 g2
    | GNew (x1, xt1, g1), GNew (x2, xt2, g2) ->
      unify_vs_types xt1 xt2 >>
      unify_gr_internal gvar_pairs g1 g2

    (* QUESTION: can we make a function "unify_vs_in_gr" which is similar to this function
      but unifies vertex structures?
      OR should we return both a gr_subst and a vs_subst from this function *)
    | GFut (g1, _), GFut (g2, _) -> unify_gr_internal gvar_pairs g1 g2
    | GTouch (v1), GTouch (v2) ->
      unify_vs v1 v2 >>
      return Subst.empty
    | GApp (g1, v11, v12), GApp (g2, v21, v22) ->
      unify_vs v11 v21 >>
      unify_vs v12 v22 >>
      unify_gr_internal gvar_pairs g1 g2
    | _, _ -> fail_cannot_unify
  end

and unify_gr gr1 gr2 = unify_gr_internal [] gr1 gr2

and unify_gr_substs (s1 : gr_subst) (s2 : gr_subst) : gr_subst grchecker =
  let fail_cannot_unify g1 g2 = fail @@ GrErrCannotUnifyGrs (g1, g2) in
  unify_substs unify_gr substitute_gr fail_cannot_unify equal_g_graph s1 s2

and unify_gr_substs_many (ss : gr_subst list) : gr_subst grchecker =
  foldM unify_gr_substs Subst.empty ss

let rec unify_vs_in_gr (g1 : g_graph) (g2 : g_graph) : vs_subst grchecker =
  inspect (UnifyVSinGrs (g1, g2)) begin
    let fail_cannot_unify = fail @@ GrErrCannotUnifyGrs (g1, g2) in
    match g1, g2 with
    | GEmpty, GEmpty -> return Subst.empty
    | GVar x, GVar y -> return Subst.empty
    | GUVar i, x | x, GUVar i -> return Subst.empty
    | GOr (g11, g12), GOr (g21, g22)
    | GSeq (g11, g12), GSeq (g21, g22)
    | GPar (g11, g12), GPar (g21, g22) -> unify_vs_in_gr g1 g2
    | GPi (_, _, _, _, g1), GPi (_, _, _, _, g2)
    | GRec (_, g1), GRec (_, g2)
    | GNew (_, _, g1), GNew (_, _, g2) -> unify_vs_in_gr g1 g2
    | GFut (g1, _), GFut (g2, _) -> unify_vs_in_gr g1 g2
    | GTouch v1, GTouch v2 -> unify_vs v1 v2
    | GApp (g1, v11, v12), GApp (g2, v21, v22) ->
      let* s1 = unify_vs_in_gr g1 g2 in
      let* s2 = unify_vs v11 v21 in
      let* s3 = unify_vs v12 v22 in
      unify_vs_substs_many [s1; s2; s3]
    | _, _ -> fail_cannot_unify
  end

let rec unify_gr_and_vs_in_typ (ty1 : g_typ) (ty2 : g_typ) : (gr_subst * vs_subst) grchecker =
  inspect (UnifyVSandGrInTys (ty1, ty2)) begin
    let fail_cannot_unify = fail @@ GrErrCannotUnifyTys (ty1, ty2) in
    match ty1.tdesc, ty2.tdesc with
    | TUVar _, TUVar _ -> fail GrErrResidualTyUnifVar
    | TVar _, TVar _ -> return (Subst.empty, Subst.empty)
    | TArrow (i1, o1, _, _, g1), TArrow (i2, o2, _, _, g2) ->
      let* (gr_subst_i, vs_subst_i) = unify_gr_and_vs_in_typ i1 i2 in
      let* (gr_subst_o, vs_subst_o) = unify_gr_and_vs_in_typ o1 o2 in
      let* gr_subst_g = unify_gr g1 g2 in
      let* gr_subst = unify_gr_substs_many [gr_subst_i; gr_subst_o; gr_subst_g] in
      let* vs_subst = unify_vs_substs_many [vs_subst_i; vs_subst_o] in
      return (gr_subst, vs_subst)
    | TConstr (id1, ts1, v1), TConstr (id2, ts2, v2) ->
       if S.compare id1 id2 != 0 then fail_cannot_unify else
         let* vs_subst = unify_vs v1 v2 in
      let* ts = try return @@ List.combine ts1 ts2
        with Invalid_argument _ -> fail_cannot_unify in
      let* substs = mapM (fun (t1, t2) -> unify_gr_and_vs_in_typ
                                            (substitute_vs_in_ty vs_subst t1)
                                            (substitute_vs_in_ty vs_subst t2))
                      ts
      in
      let* gr_subst = unify_gr_substs_many (List.map fst substs) in
      let* vs_subst = unify_vs_substs_many (vs_subst::(List.map snd substs)) in
      return (gr_subst, vs_subst)
    | TFuture(v1, t1), TFuture(v2, t2) ->
      let* vs_subst1 = unify_vs v1 v2 in
      let* gr_subst, vs_subst2 = unify_gr_and_vs_in_typ
                                   (substitute_vs_in_ty vs_subst1 t1)
                                   (substitute_vs_in_ty vs_subst1 t2)
      in
      let* vs_subst = unify_vs_substs vs_subst1 vs_subst2 in
      return (gr_subst, vs_subst)
    | TProd ts1, TProd ts2 ->
      let* ts = try return @@ List.combine ts1 ts2
                with Invalid_argument _ -> fail_cannot_unify in
      foldM (fun (gr_subst, vs_subst) (t1, t2) ->
          (pprint (fun f _ -> Format.fprintf f
                               "subbing %a\n"
                               (pprint_int_bindings pprint_g_vs)
                               (Subst.bindings vs_subst))
             ();
          let* (gr_subst', vs_subst') =
            unify_gr_and_vs_in_typ
              (substitute_gr_vs_in_ty gr_subst vs_subst t1)
              (substitute_gr_vs_in_ty gr_subst vs_subst t2)
          in
          let* gr_subst = unify_gr_substs gr_subst gr_subst' in
          let* vs_subst = unify_vs_substs vs_subst vs_subst' in
          return (gr_subst, vs_subst)))
        (Subst.empty, Subst.empty)
        ts
    | TRef t1, TRef t2 -> unify_gr_and_vs_in_typ t1 t2
    | TRecType cs1, TRecType cs2 -> unimplemented ""
    | _ -> fail_cannot_unify
    end

let rec decorate_with (t: t_typ) (s: g_schema) : g_typ grchecker =
  let rec decorate_tt (tt: t_typ) (gt: g_typ) : g_typ grchecker =
    let wrap_ty td =
      return
        {tloc = gt.tloc;
         tdesc = td
        }
    in
    match tt.tdesc, gt.tdesc with
    | TUVar i1, _ ->
       fail @@ GrErrResidualTyUnifVar
    | TVar _, _ | _, TVar _ ->
       (* XXX Shouldn't have vertices so this should be OK*)
       t_typ_to_g_typ tt
    (* | TVar x, TVar y when x = y -> wrap_ty gt.tdesc *)
    | TRef tt, TRef gt ->
       let* t = decorate_tt tt gt in
       wrap_ty @@ TRef t
    | TArrow (tt1, tt2, _, _, _),
      TArrow (gt1, gt2, spawn, touch, g) ->
       let* t1 = decorate_tt tt1 gt1 in
       let* t2 = decorate_tt tt2 gt2 in
       wrap_ty @@ TArrow (t1, t2, spawn, touch, g)
    | TProd tts, TProd gts ->
       let* ts = map2M decorate_tt tts gts in
       wrap_ty @@ TProd ts
    | TFuture (_, tt), TFuture (v, gt) ->
       let* t = decorate_tt tt gt in
       wrap_ty @@ TFuture (v, t)
    | TConstr (id1, tts, _), TConstr (id2, gts, v) when id1 = id2 ->
       let* ts = map2M decorate_tt tts gts in
       wrap_ty @@ TConstr (id1, ts, v)
    | TRecType tcons, TRecType gcons ->
       let* cons = map2M
                     (fun (conid1, tt) (conid2, gt) ->
                       if conid1 = conid2 then
                         let* t = decorate_tt tt gt in
                         return (conid1, t)
                       else
                         fail GrErrResidualTyUnifVar
                     )
                     tcons
                     gcons
       in
       wrap_ty @@ TRecType cons
    | _ ->
       let* gt1 = t_typ_to_g_typ tt in
       fail @@ GrErrCannotUnifyTys (gt1, gt)
  in
  let rec find_ty s =
    match s with
    | SMono gt -> decorate_tt t gt
    | SForall (_, s) | SForallG (_, s) -> find_ty s
  in
  find_ty s

exception NotAffine

let rec simplify_vs v = match v with
| VSVar _ -> v
| VSUVar _ -> v
| VSTuple vs -> VSTuple (List.map simplify_vs vs)
| VSProj (v, i, t) ->
   (match simplify_vs v with
    | VSTuple vs -> List.nth vs i
    | v -> VSProj (v, i, t))

let rec vertex_matches pattern v = match v with
| VSVar v -> pattern = v
| VSUVar _ -> false
| VSTuple _ -> false
| VSProj (v, _, _) -> vertex_matches pattern v

let remove_matches pattern vs = List.filter (fun x -> not @@ vertex_matches pattern x) vs

let rec vsvars_of_vs vs =
  match vs with
  | VSVar _ | VSUVar _ | VSProj _ -> [vs]
  | VSTuple vss -> List.concat (List.map vsvars_of_vs vss)

let ensure_affine xs =
  if List.length xs = List.length @@ uniq xs
    then ()
    else raise NotAffine

let rec affinity_check (gr: c_graph) = match gr with
| GEmpty -> []
| GVar _ -> []
| GUVar _ -> []
| GOr (l, r) ->
  let l_spawn = (affinity_check l) in
  let r_spawn = (affinity_check r) in
  ensure_affine l_spawn;
  ensure_affine r_spawn;
  uniq @@ l_spawn @ r_spawn
| GSeq (l, r)
| GPar (l, r) ->
  let l_spawn = (affinity_check l) in
  let r_spawn = (affinity_check r) in
  ensure_affine l_spawn;
  ensure_affine r_spawn;
  l_spawn @ r_spawn
| GFut (g, v) ->
  let spawn = affinity_check g in
  let v = simplify_vs v in
  (vsvars_of_vs v) @ spawn
| GTouch v -> []
| GPi (v1, _, _, _, g) ->
  let s = affinity_check g in
  ensure_affine s;
  let s = remove_matches v1 s in
  s
| GRec (_, g) -> affinity_check g
| GNew (v, _, g) ->
  let s = affinity_check g in
  ensure_affine s;
  let s = remove_matches v s in
  s
| GApp (g, v_spawn, _) ->
  let spawn = affinity_check g in
  let v_spawn = simplify_vs v_spawn in
  (vsvars_of_vs v_spawn) @ spawn

let affinity_check (gr: c_graph) =
  let vs_spawn = affinity_check gr in
  ensure_affine vs_spawn;
  vs_spawn


let rec grcheck (e : t_expr) : (g_graph * gr_subst * vs_subst * g_expr) grchecker =
  inspect (TExpr e) begin
    let* vst = vs_ty_of_typ e.etyp in


    let* (gr, gr_subst, vs_subst, e) = match e.edesc with
      | EVar x -> grcheck_var x e
      | EFuture ((), body) -> grcheck_future body e
      | EPar (left, right) -> grcheck_par left right e
      | EConst c -> grcheck_const c e
      | EForce body -> grcheck_force body e
      | ELet (x, t, bound, body) -> grcheck_let x t bound body e
      | EFunc (is_rec, f, x, annot1, annot2, (), (), body) ->
        grcheck_func is_rec f x annot1 annot2 body e
      | EApp (lam, _, _, arg) -> grcheck_app lam arg e
      | ETuple es -> grcheck_tuple es e
      | EIf (cond, then_branch, else_branch) -> grcheck_if cond then_branch else_branch e
      | ENewVert (_, _, _) -> fail (GrErrUnknown "ENewVert expr in grcheck")
      | EInfixop (op, e1, e2) -> grcheck_infixop op e1 e2 e
      | EMatch (scrutinee, cases) -> grcheck_match scrutinee cases e

      | ELetTuple (binders, bound, body) ->
         grcheck_let_tuple binders bound body e
      | ELetRecord (_, _, _) -> fail (GrErrUnimplemented "grcheck ELetRecord")
      | ERef body -> grcheck_ref body e
      | EDeref body -> grcheck_deref body e
      | EUpdate (e1, e2) -> grcheck_update e1 e2 e
      | ETry (_, _) -> fail (GrErrUnimplemented "grcheck ETry")
      | EAnnot (_, _) -> fail (GrErrUnimplemented "grcheck EAnnot")
    in

    (* apply vs_subst to gr_subst so that the order they are applied in does not matter *)
    let gr_subst = substitute_vs_in_gr_subst vs_subst gr_subst in

    (* pprint pprint_g_expr e;
    message "inferred graph:";
    pprint pprint_g_graph gr; *)

    return (gr, gr_subst, vs_subst, e)
  end

and rebuild (e : t_expr) edesc egr etyp : g_expr grchecker =
  return { edesc ; egr ; eloc = e.eloc ; etyp }

and grcheck_var x e =
  let* ty_ctx = get_ty_ctx in
  let* x_ty_schema =
    try
      return @@ TyCtx.find (string_of_longid x) ty_ctx
    with Not_found -> fail @@ GrErrUnknown ("unbound var " ^ (string_of_longid x))
  in
  message ("TYPE OF " ^ (string_of_longid x) ^ ":");
  pprint pprint_g_schema x_ty_schema;

  let* x_ty_schema = inst_graph x_ty_schema in
  message ("TYPE OF " ^ (string_of_longid x) ^ ":");
  pprint pprint_g_schema x_ty_schema;

  let* ty = decorate_with e.etyp x_ty_schema in
  let* e' = rebuild e (EVar x) GEmpty ty in
  return
    ( GEmpty
    , Subst.empty
    , Subst.empty
    , e' )

and typ ty =
  { tdesc = ty
  ; tloc = dloc
  }

and base_g_ty_of_string s : g_typ =
  typ (TConstr (Id s, [], (VSTuple [])))

and grcheck_const c e =
  let* (c', t) =
    match c with
    | Num n -> return @@ (Num n, base_g_ty_of_string "int")
    | String s -> return @@ (String s, base_g_ty_of_string "string")
    | Char c -> return @@ (Char c, base_g_ty_of_string "char")
    | Bool b -> return @@ (Bool b, base_g_ty_of_string "bool")
    | Unit -> return @@ (Unit, base_g_ty_of_string "unit")
    | Futref () ->
       (let* unif_idx = new_vertex_unif_var VSTVertex in
       match e.etyp.tdesc with
       | TRef ({tdesc = TFuture (_, t); _}) ->
          let* t = t_typ_to_g_typ t in
          return @@ (Futref (VSUVar unif_idx),
                     (typ @@ TRef (typ @@ TFuture (VSUVar unif_idx, t))))
       | _ -> failwith "shouldn't happen"
       )
  in
  let* e' = rebuild e (EConst c') GEmpty t in
  return (GEmpty, Subst.empty, Subst.empty, e')

and grcheck_infixop op e1 e2 e =
  let* (e1_gr, e1_gr_subst, e1_vs_subst, e1_annot) = grcheck e1 in
  let* (e2_gr, e2_gr_subst, e2_vs_subst, e2_annot) = grcheck e2 in

  let* gr_subst = unify_gr_substs e1_gr_subst e2_gr_subst in
  let* vs_subst = unify_vs_substs e1_vs_subst e2_vs_subst in

  let e1_annot = substitute_gr_vs_in_expr gr_subst vs_subst e1_annot in
  let e2_annot = substitute_gr_vs_in_expr gr_subst vs_subst e2_annot in

  let gr = GSeq (e1_gr, e2_gr) in
  (* NOTE: using t_typ_to_g_typ here is safe because infixop doesn't
     have any operations that could produce a type with any
     gr or vs unification variables *)
  let* ty = t_typ_to_g_typ e.etyp in
  let* e = rebuild e (EInfixop (op, e1_annot, e2_annot)) gr ty in

  return (gr, gr_subst, vs_subst, e)


(* let x : t = bound in body *)

(* {} *)

and grcheck_let x t bound body e =
  let* (bound_gr, bound_gr_subst, bound_vs_subst, bound_annot) = grcheck bound in
  let bound_ty = bound_annot.etyp in
  let* (body_gr, body_gr_subst, body_vs_subst, body_annot) =
    with_extended_ty_ctx x (SMono bound_ty) begin
      grcheck body
    end
  in
  let gr = GSeq (bound_gr, body_gr) in
  let* e' = rebuild e (ELet (x, t, bound_annot, body_annot)) gr body_annot.etyp in
  let gr_subst = overwrite_gr_subst body_gr_subst bound_gr_subst in
  let vs_subst = overwrite_vs_subst body_vs_subst bound_vs_subst in
  return (gr, gr_subst, vs_subst, e')

and [@warning "-8"] [@warning "-9"] [@warning "-5"] grcheck_match scrutinee cases e =

  let* (scrutinee_gr, scrutinee_gr_subst, scrutinee_vs_subst, scrutinee_annot)
    = grcheck scrutinee in

  let check_case (constructor, vars, check_body) =
    let* schema = get_con_schema (string_of_longid constructor) in
    let rec unify_ty_vars t1 t2 =
      match t1.tdesc, t2.tdesc with
      | TVar x, _ -> return [(x, t2)]
      | (TRef t1, TRef t2)
        | TFuture (_, t1), TFuture (_, t2) -> unify_ty_vars t1 t2
      | TArrow (t1a, t1r, _, _, _), TArrow (t2a, t2r, _, _, _) ->
         let* sa = unify_ty_vars t1a t2a in
         let* sr = unify_ty_vars t1r t2r in
         return @@ sa @ sr
      | TProd ts1, TProd ts2 ->
         let* ss = map2M unify_ty_vars ts1 ts2 in
         return @@ List.concat ss
      | TConstr (id1, ts1, _), TConstr (id2, ts2, _) when id1 = id2 ->
         let* ss = map2M unify_ty_vars ts1 ts2 in
         return @@ List.concat ss
      | TRecType cons1, TRecType cons2 ->
         let* ss = map2M
                     (fun (id1, t1) (id2, t2) ->
                       if id1 = id2 then
                         unify_ty_vars t1 t2
                       else
                         fail @@ GrErrCannotUnifyTys (t1, t2)
                     )
                     cons1
                     cons2
         in
         return @@ List.concat ss
    in
    let rec build_subst s =
      match s with
      | SMono ({tdesc = TArrow (_, t, _, _, _)} as ta) ->
         let* subst = unify_ty_vars t scrutinee_annot.etyp in
         return (ta, subst)
      | SForall (_, s) -> build_subst s
    in
    let rec sub_subst s t =
      {t with
        tdesc =
      match t.tdesc with
      | TVar x ->
         (try (List.assoc x s).tdesc with Not_found -> t.tdesc)
      | TRef t -> TRef (sub_subst s t)
      | TArrow (t1, t2, tp, sp, g) ->
         TArrow (sub_subst s t1, sub_subst s t2, tp, sp, g)
      | TProd ts -> TProd (List.map (sub_subst s) ts)
      | TFuture (v, t) -> TFuture (v, sub_subst s t)
      | TConstr (id, args, v) ->
         TConstr (id, List.map (sub_subst s) args, v)
      | TRecType cons ->
         TRecType (List.map (fun (id, t) -> (id, sub_subst s t)) cons)
      }
    in
    let* (arrow_ty, subst) = build_subst schema in
    let {tdesc = TArrow (t_arg, t_scr, _, (u_touch, u_touch_ty), _)} =
      sub_subst subst arrow_ty
    in
    let ts =
      match t_arg.tdesc with
      | TProd ts -> ts
      | _ -> [t_arg]
    in
    let* u_touch_unif_idx = new_vertex_unif_var u_touch_ty in
    let subst = fun t -> substitute_vert_var_in_typ t u_touch (VSUVar u_touch_unif_idx) in
    let ts = List.map subst ts in
    let t_scr' =
      substitute_vert_var_in_typ t_scr u_touch (VSUVar u_touch_unif_idx)
    in
    let* (gr_subst, vs_subst) =
      unify_gr_and_vs_in_typ t_scr' scrutinee_annot.etyp
    in

    let subst = substitute_gr_vs_in_ty gr_subst vs_subst in
    let ts = List.map subst ts in
    let _ = List.map2 (fun v t ->
                pprint (fun f () -> Format.fprintf f "adding %s with %a" v
                                      pprint_g_typ t) ())
              vars
              ts
    in
    with_extended_ty_ctx_n vars (List.map (fun x -> SMono x) ts) begin
      let* ty_ctx = get_ty_ctx in
      grcheck check_body
    end
  in

  let* outputs = mapM check_case cases in
  let case_grs = List.map (fun (x, _, _, _) -> x) outputs in
  let case_gr_substs = List.map (fun (_, x, _, _) -> x) outputs in
  let case_vs_substs = List.map (fun (_, _, x, _) -> x) outputs in
  let cases_annot = List.map (fun (_, _, _, x) -> x) outputs in

  let return_tys = List.map (fun e -> e.etyp) cases_annot in
  let* return_substs = match return_tys with
    | [] -> fail @@ GrErrUnknown "no cases in match expression"
    | x :: [] -> return [(Subst.empty, Subst.empty)]
    | x :: xs -> mapM (unify_gr_and_vs_in_typ x) xs
  in
  let return_gr_substs = List.map (fun (x, _) -> x) return_substs in
  let return_vs_substs = List.map (fun (_, x) -> x) return_substs in

  let gr = GSeq (scrutinee_gr, collect_gr_or case_grs) in
  let ty = List.hd return_tys in
  let* gr_subst = unify_gr_substs_many (scrutinee_gr_subst :: case_gr_substs @ return_gr_substs) in
  let* vs_subst = unify_vs_substs_many (scrutinee_vs_subst :: case_vs_substs @ return_vs_substs) in
  let cases_annot = List.map2 (fun (x, y, _) z -> (x, y, z)) cases cases_annot in

  let* e = rebuild e
    (EMatch (scrutinee_annot, cases_annot))
    gr
    ty
  in

  return (gr, gr_subst, vs_subst, e)

and grcheck_future body e =
  let* (body_graph, body_gr_subst, body_vs_subst, body_annot) = grcheck body in
  let* unif_idx = new_vertex_unif_var VSTVertex in
  let gr = GFut (body_graph, VSUVar unif_idx) in
  let* e' = rebuild e (EFuture (VSUVar unif_idx, body_annot)) gr
              (typ (TFuture (VSUVar unif_idx, body_annot.etyp)))
  in
  return (gr, body_gr_subst, body_vs_subst, e')

and grcheck_par left right e =
  let* (left_graph, left_gr_subst, left_vs_subst, left_annot) = grcheck left in
  let* (right_graph, right_gr_subst, right_vs_subst, right_annot) = grcheck right in
  let* e' = rebuild e
              (EPar(left_annot, right_annot))
              (GPar (left_graph, right_graph))
              (typ @@ TProd [left_annot.etyp; right_annot.etyp])
  in
  return (GPar (left_graph, right_graph), Subst.empty, Subst.empty, e')

and [@warning "-8"] [@warning "-5"] grcheck_force body e =
  let* (body_gr, gr_subst, vs_subst, body_annot) = grcheck body in
  let TFuture (vert, t_wrapped) = body_annot.etyp.tdesc in
  let gr_touch = GTouch vert in
  let gr = GSeq(body_gr, gr_touch) in
  let* e' = rebuild e (EForce body_annot) gr t_wrapped in
  return (gr, gr_subst, vs_subst, e')

and [@warning "-8"] [@warning "-5"] grcheck_func is_rec f x annot1 annot2 body e =
  let func_ty = e.etyp in
  let* func_ty = t_typ_to_g_typ func_ty in

  let* gvar = new_graph_var in

  (* Graph check the body 1st time *)
  let* (gr, ty, gr_subst, vs_subst, body') =
    grcheck_func_iter is_rec f x body func_ty gvar
  in
  (* Remember the type we get *)
  let TArrow(x_ty, body_ty, (u_spawn, u_spawn_ty), (u_touch, u_touch_ty), func_gr) = ty.tdesc in

  (* Graph check the body 2nd time using the type we got *)
  let* (_, ty', gr_subst', vs_subst', body') =
    grcheck_func_iter is_rec f x body ty gvar
  in
  (* Unpack the new type *)
  let TArrow(x_ty', body_ty', (u_spawn', u_spawn_ty'), (u_touch', u_touch_ty'), func_gr') = ty'.tdesc in

  (* Did anything change? *)
  (* We don't actually care about the substitution, we just want this
   * to fail if they don't unify *)

  let* s1 = unify_vs_types u_spawn_ty u_spawn_ty' in
  let* s2 = unify_vs_types u_touch_ty u_touch_ty' in

  let* e = rebuild e
             (EFunc (is_rec, f, x, annot1, annot2,
                     (u_spawn', u_spawn_ty'), (u_touch', u_touch_ty'),
                     body')
             )
             func_gr'
             ty'
  in
  return (GEmpty, gr_subst, vs_subst, e)

and [@warning "-8"] [@warning "-5"] grcheck_func_iter is_rec f x body func_ty gvar =

  let TArrow(x_ty, body_ty, (u_spawn, u_spawn_ty), (u_touch, u_touch_ty), func_gr) = func_ty.tdesc in
  let* (next_body, spawn_params, touch_params, vs_subst, gr_subst) =
    with_extended_ty_ctx x (SMono x_ty) begin
    let* (body_gr, body_gr_subst, body_vs_subst, body_annot) =
      match is_rec with
      | Recursive ->
         let func_ty_with_g =
           typ @@ TArrow (x_ty, body_ty,
                          (u_spawn, u_spawn_ty), (u_touch, u_touch_ty),
                          GApp(GVar gvar, VSVar u_spawn, VSVar u_touch))
         in
         with_extended_ty_ctx f (SMono func_ty_with_g) (grcheck body)
      | Terminal -> grcheck body
    in

    let body_annot =
      substitute_gr_vs_in_expr body_gr_subst body_vs_subst body_annot
    in

    (* extract vertex parameters *)
    let body_gr_vert_unif_vars = free_spawn_vert_unif_vars_expr body_annot in

    let x_ty = substitute_gr_in_ty body_gr_subst x_ty in
    let x_ty = substitute_vs_in_ty body_vs_subst x_ty in

    let body_ty = body_annot.etyp in
    let body_ty = substitute_gr_in_ty body_gr_subst body_ty in
    let body_ty = substitute_vs_in_ty body_vs_subst body_ty in

    (* vertices that appear free in the type context can stay free *)
    let* ty_ctx = get_ty_ctx in
    let ty_ctx_vert_unif_vars = free_vert_unif_vars_ty_ctx ty_ctx in
    let body_gr_vert_unif_vars =
      uniq @@ diff body_gr_vert_unif_vars ty_ctx_vert_unif_vars
    in

    (* need these to not be free *)
    (* vertices that appear in x_ty need to be touch parameters *)
    let touch_params = uniq @@ free_vert_unif_vars_ty x_ty in

    (* vertices that appear in body_ty need to be spawn parameters *)
    let body_ty_vert_unif_vars = free_vert_unif_vars_ty body_ty in
    let spawn_params =
      uniq @@ inter body_gr_vert_unif_vars body_ty_vert_unif_vars
    in

    (* vertices that appear in neither, introduce "new" binding around body_annot *)
    let new_vertices =
       (diff
          (diff
             body_gr_vert_unif_vars
             touch_params)
          spawn_params)
    in

    let* u = new_vertex_name in
    let* (next_body, vs_subst) = (
      match new_vertices with
      | [] -> return (body_annot, body_vs_subst)
      | xs ->
        let indices = List.mapi (fun i v -> (i, v)) new_vertices in
        let* vsut_ctx = get_vs_unif_typ_ctx in

        let* vst_prod_ty_unif_idx = new_vs_typ_unif_var in
        let* prod_ty, vert_idx_subst =
          collect u new_vertices
        in
        let* _ = set_vs_typ_unif_var vst_prod_ty_unif_idx prod_ty in
        let vs_typ_subst = Subst.singleton vst_prod_ty_unif_idx prod_ty in
        let vert_idx_subst = substitute_vst_in_vs_subst vs_typ_subst vert_idx_subst in

        let* vert_idx_subst = unify_vs_substs vert_idx_subst body_vs_subst in
        let next_body = substitute_vs_in_expr vert_idx_subst body_annot in
        let next_gr = substitute_vs_in_gr vert_idx_subst body_gr in
        (match prod_ty with
         | VSTProd [] -> return (next_body, vert_idx_subst)
         | _ ->
            let* next_body = rebuild body
                               (ENewVert (u, prod_ty, next_body))
                               (GNew (u, prod_ty, next_body.egr))
                               body_ty
            in
            return (next_body, vert_idx_subst)
        )
    ) in
    return (next_body, spawn_params, touch_params, vs_subst, body_gr_subst)
  end in

  let* (spawn_ty, next_body, vs_subst) = begin
      let* vsut_ctx = get_vs_unif_typ_ctx in
      let* prod_ty, vert_idx_subst =
          collect u_spawn spawn_params
      in
      let* vert_idx_subst = unify_vs_substs vert_idx_subst vs_subst in
      let next_body = substitute_vs_in_expr vert_idx_subst next_body in
      return (prod_ty, next_body, vert_idx_subst)
    end
  in

  let next_gr =
    GApp (
        GRec(gvar,
             GPi (u_spawn, spawn_ty, u_touch, u_touch_ty, next_body.egr))
        , VSVar u_spawn
        , VSVar u_touch)
  in
  let next_ty = TArrow (x_ty, next_body.etyp, (u_spawn, spawn_ty), (u_touch, u_touch_ty), next_gr) in

  return (next_gr, InferTy.typ next_ty, gr_subst, vs_subst, next_body)

and [@warning "-8"] [@warning "-5"] grcheck_app lam arg e =

  let* (lam_gr, lam_gr_subst, lam_vs_subst, lam_annot) = grcheck lam in

  let* (arg_gr, arg_gr_subst, arg_vs_subst, arg_annot) = grcheck arg in

  let* gr_subst = unify_gr_substs lam_gr_subst arg_gr_subst in
  let* vs_subst = unify_vs_substs lam_vs_subst arg_vs_subst in

  let TArrow (t1, t2, (u_spawn, u_spawn_ty), (u_touch, u_touch_ty), lam_exec_gr) = lam_annot.etyp.tdesc in

  let* unif_spawn = new_vertex_unif_var u_spawn_ty in
  let* unif_touch = new_vertex_unif_var u_touch_ty in
  let t1 = substitute_vert_var_in_typ t1 u_spawn (VSUVar unif_spawn) in
  let t1 = substitute_vert_var_in_typ t1 u_touch (VSUVar unif_touch) in
  let t2 = substitute_vert_var_in_typ t2 u_spawn (VSUVar unif_spawn) in
  let t2 = substitute_vert_var_in_typ t2 u_touch (VSUVar unif_touch) in

  let* typ_gr_subst, typ_vs_subst = unify_gr_and_vs_in_typ t1 arg_annot.etyp in

  let* gr_subst = unify_gr_substs gr_subst typ_gr_subst in

  let* vs_subst = unify_vs_substs vs_subst typ_vs_subst in

  let spawn_param = try Subst.find unif_spawn typ_vs_subst
    with Not_found -> VSUVar unif_spawn in

  let touch_param = try Subst.find unif_touch typ_vs_subst
                    with Not_found -> VSUVar unif_touch in

  let lam_gr = substitute_gr gr_subst lam_gr in
  let lam_gr = substitute_vs_in_gr vs_subst lam_gr in
  let lam_annot = substitute_gr_in_expr gr_subst lam_annot in
  let lam_annot = substitute_vs_in_expr vs_subst lam_annot in
  let arg_gr = substitute_gr gr_subst arg_gr in
  let arg_gr = substitute_vs_in_gr vs_subst arg_gr in
  let arg_annot = substitute_gr_in_expr gr_subst arg_annot in
  let arg_annot = substitute_vs_in_expr vs_subst arg_annot in

  let lam_exec_gr = substitute_vert_var_in_gr lam_exec_gr u_spawn spawn_param in
  let lam_exec_gr = substitute_vert_var_in_gr lam_exec_gr u_touch touch_param in

  let gr = GSeq (lam_gr, GSeq (arg_gr, lam_exec_gr)) in

  let t2 = substitute_vert_var_in_typ t2 u_spawn spawn_param in
  let t2 = substitute_vert_var_in_typ t2 u_touch touch_param in

  let* e = rebuild e
    (EApp (lam_annot, spawn_param, touch_param, arg_annot))
    gr
    t2
  in

  return (gr, gr_subst, vs_subst, e)

and grcheck_tuple es e =
  let* results = all @@ List.map grcheck es in
  let grs = List.map (fun (x, _, _, _) -> x) results in
  let gr_substs = List.map (fun (_, x, _, _) -> x) results in
  let vs_substs = List.map (fun (_, _, x, _) -> x) results in
  let es_annot = List.map (fun (_, _, _, x) -> x) results in

  let gr = build_seq grs in
  let* gr_subst = unify_gr_substs_many gr_substs in
  let* vs_subst = unify_vs_substs_many vs_substs in

  let t = {e.etyp with tdesc = TProd (List.map (fun e -> e.etyp) es_annot)} in
  let t = substitute_gr_in_ty gr_subst t in
  let t = substitute_vs_in_ty vs_subst t in

  let es_annot = List.map (substitute_gr_vs_in_expr gr_subst vs_subst) es_annot
  in
  let* e = rebuild e (ETuple es_annot) gr t in

  return (gr, gr_subst, vs_subst, e)

and grcheck_if cond then_branch else_branch e =

  let* (cond_gr, cond_gr_subst, cond_vs_subst, cond_annot) = grcheck cond in
  let* (then_gr, then_gr_subst, then_vs_subst, then_annot) = grcheck then_branch in
  let* (else_gr, else_gr_subst, else_vs_subst, else_annot) = grcheck else_branch in

  let* gr_subst = unify_gr_substs_many [cond_gr_subst; then_gr_subst; else_gr_subst] in
  let* vs_subst = unify_vs_substs_many [cond_vs_subst; then_vs_subst; else_vs_subst] in
  let gr_subst = substitute_vs_in_gr_subst vs_subst gr_subst in

  let gr = GSeq (cond_gr, GOr (then_gr, else_gr)) in

  (* we already know these types have been unified in typechecking *)
  let t1 = then_annot.etyp in
  let t2 = else_annot.etyp in
  (* so it suffices to just unify their graphs and vertex structures *)
  let* ty_gr_subst, ty_vs_subst = unify_gr_and_vs_in_typ t1 t2 in

  let* gr_subst = unify_gr_substs gr_subst ty_gr_subst in
  let* vs_subst = unify_vs_substs vs_subst ty_vs_subst in

  let t1 = substitute_gr_vs_in_ty ty_gr_subst ty_vs_subst t1 in
  let t2 = substitute_gr_vs_in_ty ty_gr_subst ty_vs_subst t2 in

  let cond_gr = substitute_gr_vs_in_gr gr_subst vs_subst cond_gr in
  let then_gr = substitute_gr_vs_in_gr gr_subst vs_subst then_gr in
  let else_gr = substitute_gr_vs_in_gr gr_subst vs_subst else_gr in

  let cond_annot = substitute_gr_vs_in_expr gr_subst vs_subst cond_annot in
  let then_annot = substitute_gr_vs_in_expr gr_subst vs_subst then_annot in
  let else_annot = substitute_gr_vs_in_expr gr_subst vs_subst else_annot in

  let* e = rebuild e
    (EIf (cond_annot, then_annot, else_annot))
    gr
    t1
  in

  return (gr, gr_subst, vs_subst, e)

and grcheck_let_tuple binders bound body e =
  let* (bd_gr, bd_gr_subst, bd_vs_subst, bd_annot) = grcheck bound in
  let bound_ty = bd_annot.etyp in
  let* tys =
    match bound_ty.tdesc with
    | TProd tys -> return tys
    | _ -> fail @@ GrErrUnknown "let_tuple not a tuple"
  in
  let rec do_substs binders bound_tys =
    match (binders, bound_tys) with
    | ([], []) -> let* x = grcheck body in return x
    | (binder::binders, bound_ty::bound_tys) ->
        let* x = with_extended_ty_ctx binder (SMono bound_ty)
          (do_substs binders bound_tys) in
        return x
    | _ -> fail @@ GrErrUnknown "let_tuple invalid tuple"
  in
  let* (body_gr, body_gr_subst, body_vs_subst, body_annot) =
    do_substs binders tys
  in
  let* gr_subst = unify_gr_substs bd_gr_subst body_gr_subst in
  let* vs_subst = unify_vs_substs bd_vs_subst body_vs_subst in
  let gr = build_seq [bd_gr; body_gr] in
  let* e = rebuild e (ELetTuple (binders, bd_annot, body_annot)) gr
             body_annot.etyp
  in
  return (
      gr,
      gr_subst,
      vs_subst,
      e
    )

and grcheck_ref body e =
  let* (body_gr, body_gr_subst, body_vs_subst, body_annot) = grcheck body in
  let* e = rebuild e (ERef body_annot) body_gr (typ (TRef body_annot.etyp)) in
  return (body_gr, body_gr_subst, body_vs_subst, e)

and grcheck_deref body e =
  let* (body_gr, body_gr_subst, body_vs_subst, body_annot) = grcheck body in
  let* ty =
    match body_annot.etyp.tdesc with
    | TRef t -> return t
    | _ -> fail @@ GrErrUnknown "deref not a ref"
  in
  let* e = rebuild e (EDeref body_annot) body_gr ty in
  return (body_gr, body_gr_subst, body_vs_subst, e)

and grcheck_update ref_e val_e e =
  let* (ref_gr, ref_gr_subst, ref_vs_subst, ref_annot) = grcheck ref_e in
  let* ref_ty =
    match ref_annot.etyp.tdesc with
    | TRef t -> return t
    | _ -> fail @@ GrErrUnknown "deref not a ref"
  in
  let* (val_gr, val_gr_subst, val_vs_subst, val_annot) = grcheck val_e in
  let* gr_subst = unify_gr_substs ref_gr_subst val_gr_subst in
  let* vs_subst = unify_vs_substs ref_vs_subst val_vs_subst in
  let* (gr_subst, vs_subst) =
    unify_gr_and_vs_in_typ ref_ty val_annot.etyp
  in
  let ref_annot = substitute_gr_vs_in_expr gr_subst vs_subst ref_annot in
  let val_annot = substitute_gr_vs_in_expr gr_subst vs_subst val_annot in
  let gr = build_seq [ref_gr; val_gr] in
  let gr = substitute_gr_vs_in_gr gr_subst vs_subst gr in
  let* unit_ty = t_typ_to_g_typ (base_ty_of_string "unit") in
  let* e = rebuild e (EUpdate (ref_annot, val_annot)) gr unit_ty
  in
  return (gr, gr_subst, vs_subst, e)


let rec replace_touch_unif_vars (vs: g_vertex_struct)
        : g_vertex_struct grchecker =
    match vs with
  | VSVar x -> return @@ VSVar x
  | VSUVar x ->
     let* vsut_ctx = get_vs_unif_typ_ctx in
     message @@ "type of $" ^ (string_of_int x);
     pprint pprint_g_vs_typ (VSUTCtx.find x vsut_ctx) ;
     (match VSUTCtx.find x vsut_ctx with
      | VSTProd [] -> return @@ VSTuple []
      | _ -> return @@ VSUVar x
     )
  | VSTuple x ->
     let* ts = mapM replace_touch_unif_vars x in
     return @@ VSTuple ts
  | VSProj (x, i, vst) ->
     let* x = replace_touch_unif_vars x in
     return @@ VSProj (x, i, vst)

let rec finalize_vs_typ (vst: g_vs_typ) : c_vs_typ grchecker =
  inspect (VST vst) begin
  match vst with
  | VSTVertex -> return VSTVertex
  | VSTVar v -> return @@ VSTVar v
  | VSTUVar i -> fail @@ GrErrUnknown ("can't generalize vert type unif var "
                                   ^ (string_of_int i))
  | VSTProd ts ->
     let* ts = mapM finalize_vs_typ ts in
     return @@ VSTProd ts
  | VSTCoRec (t, vst) ->
     let* vst = finalize_vs_typ vst in
     return @@ VSTCoRec (t, vst)
    end

let rec finalize_vs (vs: g_vertex_struct) : c_vertex_struct grchecker =
  inspect (VS vs) begin
  match vs with
  | VSVar x -> return @@ VSVar x
  | VSUVar x ->
     let* vsut_ctx = get_vs_unif_typ_ctx in
     (match VSUTCtx.find x vsut_ctx with
      | VSTProd [] -> return @@ VSTuple []
      | _ -> fail @@ GrErrUnknown ("can't generalize vert unif var "
                                   ^ (string_of_int x))
     )
  | VSTuple x ->
     let* ts = mapM finalize_vs x in
     return @@ VSTuple ts
  | VSProj (x, i, t) ->
     let* x = finalize_vs x in
     let* vst = return @@ VSTProd [] in
     return @@ VSProj (x, i, vst)
    end

let rec transform_graph_monadic
          transform_unif_var
          transform_vs
          transform_vs_typ
          g =
  inspect (Graph g) begin
  let recur = transform_graph_monadic
                transform_unif_var transform_vs transform_vs_typ
  in
  match g with
  | GEmpty -> return GEmpty
  | GVar x -> return @@ GVar x
  | GUVar x ->
     let* x = transform_unif_var x in
     return @@ GUVar x
  | GSeq (x, y) ->
     let* g1 = recur x in
     let* g2 = recur y in
     return @@ GSeq (g1, g2)
  | GOr (x, y) ->
     let* g1 = recur x in
     let* g2 = recur y in
     return @@ GOr (g1, g2)
  | GPar (x, y) ->
     let* g1 = recur x in
     let* g2 = recur y in
     return @@ GPar (g1, g2)
  | GFut (x, y) ->
     let* vs = transform_vs y in
     let* g = recur x in
     return @@ GFut (g, vs)
  | GTouch x ->
     let* vs = transform_vs x in
     return @@ GTouch vs
  | GPi (uf, uft, ut, utt, g) ->
     let* uft = transform_vs_typ uft in
     let* utt = transform_vs_typ utt in
     let* g = recur g in
     return @@ GPi (uf, uft, ut, utt, g)
  | GRec (x, y) ->
     let* g = recur y in
     return @@ GRec (x, g)
  | GNew (u, ut, g) ->
     let* ut = transform_vs_typ ut in
     let* g = recur g in
     return @@ GNew (u, ut, g)
  | GApp (g, uf, ut) ->
     let* uf = transform_vs uf in
     let* ut = transform_vs ut in
     let* g = recur g in
     return @@ GApp (g, uf, ut)
    end

let rec transform_const_monadic f const
  = match const with
  | Num n -> return @@ Num n
  | String s -> return @@ String s
  | Char c -> return @@ Char c
  | Bool b -> return @@ Bool b
  | Unit -> return Unit
  | Futref x -> let* x = f x in return @@ Futref x

let rec transform_expr_monadic f1 f2 f3 f4 f5 (f6: (vertex_var * g_vs_typ) -> (vertex_var * c_vs_typ) grchecker) f7 expr :
          c_expr
            grchecker
 =
  let recur = transform_expr_monadic f1 f2 f3 f4 f5 f6 f7 in
  let handle_matches =
    mapM (fun (x, y, z) -> let* z = recur z in return (x, y, z))
  in
  let rebuild edesc =
    let* etyp = f3 expr.etyp in
    let* egr = f4 expr.egr in
    return @@ {expr with edesc; etyp; egr}
  in
  let* new_exp =
    match expr.edesc with
  | EVar x -> return @@ EVar x
  | EConst x ->
     let* x = transform_const_monadic f5 x in
     return @@ EConst x
  | EInfixop (x, y, z) ->
     let* y = recur y in
     let* z = recur z in
     return @@ EInfixop (x, y, z)
  | EIf (x, y, z) ->
     let* x = recur x in
     let* y = recur y in
     let* z = recur z in
     return @@ EIf (x, y, z)
  | ELet (x, y, z, w) ->
     let* z = recur z in
     let* w = recur w in
     return @@ ELet (x, y, z, w)
  | EFunc (a, b, c, d, e, f, g, h) ->
     let* f : vertex_var * c_vs_typ = f6 f in
     let* g : vertex_var * c_vs_typ = f6 g in
     let* h = recur h in
     return @@ EFunc (a, b, c, d, e, f, g, h)
  | ELetTuple (x, y, z) ->
     let* y = recur y in
     let* z = recur z in
     return @@ ELetTuple (x, y, z)
  | ELetRecord (x, y, z) ->
     let* y = recur y in
     let* z = recur z in
     return @@ ELetRecord (x, y, z)
  | EApp (x, y, z, w) ->
     let* x = recur x in
     let* y = f5 y in
     let* z = f5 z in
     let* w = recur w in
     return @@ EApp (x, y, z, w)
  | EMatch (x, y) ->
     let* x = recur x in
     let* y = handle_matches y in
     return @@ EMatch (x, y)
  | ETuple x ->
     let* x = mapM recur x in
     return @@ ETuple x
  | ERef x -> let* x = recur x in return @@ ERef x
  | EDeref x -> let* x = recur x in return @@ EDeref x
  | EUpdate (x, y) ->
     let* x = recur x in
     let* y = recur y in
     return @@ EUpdate (x, y)
  | EFuture (x, y) ->
     let* x = f5 x in
     let* y = recur y in
     return @@ EFuture (x, y)
  | EForce x -> let* x = recur x in return @@ EForce x
  | EPar (x, y) ->
     let* x = recur x in
     let* y = recur y in
     return @@ EPar (x, y)
  | ETry (x, y) ->
     let* x = recur x in
     let* y = handle_matches y in
     return @@ ETry (x, y)
  | EAnnot (x, y) -> let* x = recur x in return @@ EAnnot (x, y)
  | ENewVert (x, y, z) ->
     let* y = f7 y in
     let* z = recur z in
     return @@ ENewVert (x, y, z)
  in
  rebuild new_exp


let replace_touch_unif_vars_in_gr =
  transform_graph_monadic
    idM
    replace_touch_unif_vars
    idM

let replace_touch_unif_vars_in_typ =
  transform_typ_monadic
    idM
    replace_touch_unif_vars_in_gr
    replace_touch_unif_vars
    idM
    idM

let finalize_graph g =
  message "Finalize:";
  pprint pprint_g_graph g;
  let* g = replace_touch_unif_vars_in_gr g in
  let* g =
    transform_graph_monadic
      (fun i -> failwith ("can't generalize graph unif var $" ^ (string_of_int i)))
      finalize_vs
      finalize_vs_typ
      g
  in
  let g = rewrite_gr g in
  return g

let rec external_to_g_typ (ty : t_typ) : g_typ grchecker =
  let* tdesc = match ty.tdesc with
  | TVar x -> return @@ TVar x
  | TUVar _ -> fail @@ GrErrUnknown "ty unif var in external type"
  | TRef t ->
    let* t = external_to_g_typ t in
    return @@ TRef t
  | TArrow (i, o, _, _, _) ->
    let* i = external_to_g_typ i in
    let* o = external_to_g_typ o in
    let* v1 = new_vertex_name in
    let* v2 = new_vertex_name in
    let g = GPi (v1, VSTProd [], v2, VSTProd [], GEmpty) in
    return @@ TArrow (i, o, (v1, VSTProd []), (v2, VSTProd []), GApp(g, VSTuple [], VSTuple []))
  | TProd ts ->
    let* ts = mapM external_to_g_typ ts in
    return @@ TProd ts
  | TFuture (_, t) ->
    let* t = external_to_g_typ t in
    return @@ TFuture (VSTuple [], t)
  | TConstr (x, ts, _) ->
    let* ts = mapM external_to_g_typ ts in
    return @@ TConstr (x, ts, VSTuple [])
  | TRecType xs ->
    let f (x, t) =
      let* t = external_to_g_typ t in
      return (x, t)
    in
    let* xs = mapM f xs in
    return @@ TRecType xs
  in
  return { ty with tdesc }

let finalize_typ t =
  message "Finalize: ";
  pprint pprint_g_typ t;
  transform_typ_monadic
    (fun _ -> failwith "can't generalize unif var")
    finalize_graph
    finalize_vs
    (fun (v, vst) -> let* vst = finalize_vs_typ vst in return @@ (v, vst))
    finalize_vs_typ
    t

let finalize_expr e : c_expr grchecker =
  message "Finalize: ";
  pprint pprint_g_expr e;
  transform_expr_monadic
    (fun _ -> failwith "can't generalize unif var")
    (fun _ -> failwith "can't generalize graph unif var")
    finalize_typ
    finalize_graph
    finalize_vs
    (fun (v, vst) -> let* vst = finalize_vs_typ vst in return @@ (v, vst))
    finalize_vs_typ
    e

let rec finalize_schema s =
  match s with
  | SMono t -> let* t = finalize_typ t in return @@ SMono t
  | SForall (a, s) ->
     let* s = finalize_schema s in
     return @@ SForall (a, s)
  | SForallG (g, s) ->
     let* s = finalize_schema s in
     return @@ SForallG (g, s)

let finalize_decl d =
  let rebuild desc =
    let* t = finalize_schema d.dinfo in
    return {d with ddesc = desc; dinfo = t}
  in
  match d.ddesc with
  | DVal (s, t, e) ->
     let* e = finalize_expr e in
     rebuild @@ DVal (s, t, e)
  | DExp e ->
     let* e = finalize_expr e in
     rebuild @@ DExp e
  | DExtType s -> rebuild @@ DExtType s
  | DExtRecType (s, c) -> rebuild @@ DExtRecType (s, c)
  | DExternal (s, t) -> rebuild @@ DExternal (s, t)
  | DTypeDef (a, b, c) -> rebuild @@  DTypeDef (a, b, c)

let grcheck_decl (decl : t_decl) after =

  let rebuild_d t desc =
    {decl with ddesc = desc; dinfo = t}
  in

  let rec replace_schema s t  =
    match s with
    | SMono _ -> t
    | SForall (x, next) -> SForall (x, replace_schema next t)
    | SForallG (x, next) -> SForallG (x, replace_schema next t)
  in

  inspect (TDecl decl) begin
    message "declaration: ";
    pprint pp_t_decl decl;

    let _ = decl.dinfo in

    let numoutputs : int VMap.t ref = ref VMap.empty in

    let dotfilename s =
      let basename = if s = "-" || s = "_" then "anon"
                     else s
      in
      let num =
        (try
           VMap.find (Id basename) (!numoutputs)
         with Not_found -> 0
        )
      in
      numoutputs := VMap.add (Id basename) (num + 1) (!numoutputs);
      basename ^ (if num = 0 then "" else string_of_int num) ^ ".dot"
    in

    let continue global_verts name (gr: g_graph) e d =
      let typ = transform_typ id rewrite_gr id id e.etyp in
      let* typ = replace_touch_unif_vars_in_typ typ in
      let* schema, gr_subst = generalize typ in
      let typ = extract_ty_from_schema schema in
      let d = substitute_gr_in_decl gr_subst d in

      let* c_d : c_decl = finalize_decl d in

      let schema = replace_schema schema (replace_schema decl.dinfo (SMono typ))
      in
      let* (prog, global_verts', ctx, ty_ctx) =
        with_extended_ty_ctx name schema begin
          with_extended_gr_ctx name gr after
        end
      in
      return (c_d::prog,
              VSCtx.union (fun _ a _ -> Some a) global_verts global_verts',
              ctx, ty_ctx)
    in

    match decl.ddesc with
    | DVal (name, t, body) ->
      let* (g, _, _, e) = grcheck body in
      let diff a b = List.filter (fun x -> not @@ List.mem x b) a in
      let rec uniq l =
        match l with
        | [] -> []
        | h::t -> h::(uniq (List.filter ((<>) h) t))
      in

      (* vertices that appear free in the type context can stay free *)
      let* ty_ctx = get_ty_ctx in
      let body_gr_vert_unif_vars = free_spawn_vert_unif_vars_expr e in
      let ty_ctx_vert_unif_vars = free_vert_unif_vars_ty_ctx ty_ctx in
      let new_vertices =
        uniq @@ diff body_gr_vert_unif_vars ty_ctx_vert_unif_vars
      in
      let* (verts, g, e) = (
          match new_vertices with
          | [] -> return (VSCtx.empty, g, e)
          | _ ->
             let* u = new_vertex_name in
             let* vsut_ctx = get_vs_unif_typ_ctx in
             let* vst_prod_ty_unif_idx = new_vs_typ_unif_var in
             let* prod_ty, vert_idx_subst =
               collect u new_vertices
             in
             (match prod_ty with
              | VSTProd [] -> return (VSCtx.empty, g, e)
              | _ ->
                 message "new subst:";
                 pprint (pprint_int_bindings pprint_g_vs) (Subst.bindings vert_idx_subst);
                 pprint pprint_g_vs_typ prod_ty;
                 let* _ = set_vs_typ_unif_var vst_prod_ty_unif_idx prod_ty in
                 let vs_typ_subst = Subst.singleton vst_prod_ty_unif_idx prod_ty in
                 let vert_idx_subst = substitute_vst_in_vs_subst vs_typ_subst vert_idx_subst in

                 let next_body = substitute_vs_in_expr vert_idx_subst e in
                 let next_gr = substitute_vs_in_gr vert_idx_subst g in
                 message "new subst:";
                 pprint (pprint_int_bindings pprint_g_vs) (Subst.bindings vert_idx_subst);
                 let* next_body = rebuild body
                                    next_body.edesc
                                    next_gr
                                    next_body.etyp
                 in
                 let* ty = finalize_vs_typ prod_ty in
                 return (VSCtx.singleton u ty, next_gr, next_body)
             )
        )
      in
      let d_schema =
        replace_schema decl.dinfo (SMono e.etyp)
      in
      pprint (pair pprint_g_graph pprint_g_expr "\n") (g, e);
      continue verts name g e (rebuild_d d_schema (DVal (name, t, e)))

    | DExternal (Id name, pt) ->
      let* ty : g_schema = transform_schema_monadic external_to_g_typ decl.dinfo in
      let transform st = {
        st with
        ty_ctx = TyCtx.add name ty st.ty_ctx;
        gr_ctx = GrCtx.add name GEmpty st.gr_ctx;
        }
      in
      with_transformed_state transform after

    | DExtType _ -> after

    | DTypeDef (tyvars, name, constructors) ->
       message "grcheck typedef";
     let* tyname_ctx = get_tyname_ctx in
     let* con_ctx = get_con_ctx in
     let* tyctx = get_ty_ctx in
     let constructors =
       List.map
         (fun (c, t) ->
           let [@warning "-8"] SMono t = InferTy.trust_p_typ t in
           (c, t)
         )
         constructors
     in
     let* (tyname_ctx, con_ctx, tyctx) =
       add_rec_ty_to_ctx name tyvars constructors
         (tyname_ctx, con_ctx, tyctx)
     in
     with_transformed_state
       (fun st -> { st with tyname_ctx = tyname_ctx
                          ; con_ctx = con_ctx
                          ; ty_ctx = tyctx })
       after

    | _ -> fail (GrErrUnimplemented "grcheck decl")
  end

let empty_grcheck_state =
  { inspection_stack = []
  ; next_graph_unif_var_num = ref 1
  ; next_vertex_unif_var_num = ref 1
  ; next_vertex_id = ref 1
  ; next_vs_typ_unif_var_num = ref 1
  ; next_graph_id = ref 1
  ; vs_unif_typ_ctx = ref VSUTCtx.empty
  ; gr_ctx = GrCtx.empty
  ; spawn_ctx = VSCtx.empty
  ; touch_ctx = VSCtx.empty
  ; ty_ctx = TyCtx.empty
  ; tyname_ctx = InferTy.Ctx.empty (* Need to fill in with ctx from tychecking *)
  ; con_ctx = TyCtx.empty (* This will need to be filled in *)
  }

let (st, (tyname_ctx, con_ctx, tyctx)) =
  let typ = InferTy.typ in
  List.fold_left (fun (s, d) (a, b, c) ->
      match
        (let* d = add_rec_ty_to_ctx a b c d in
         let* st = get_state in
         return (st, d))
        s
      with Ok (st, d) -> (st, d)
         | Error e -> failwith (show_gr_error e)
    )
    (empty_grcheck_state, (TyCtx.empty, TyCtx.empty, TyCtx.empty))
    [("option", ["'a"],
      [("None", typ @@ TProd []);
       ("Some", typ @@ TVar "'a")]);
     ("list", ["'a"],
      [("Nil", typ @@ TProd []);
       ("Cons", typ @@ TProd [typ @@ TVar "'a";
                              typ @@ TConstr (Id "list", [typ @@ TVar "'a"], ())])])
    ]

let empty_grcheck_state = { st with
                            tyname_ctx = tyname_ctx;
                            con_ctx = con_ctx;
                            ty_ctx = tyctx }

let grcheck_state_from_tyname_ctx tyname_ctx =
  {
    st with
    ty_ctx = con_ctx
  ; tyname_ctx = tyname_ctx
  ; con_ctx = con_ctx (* This will need to be filled in *)
  }

let grcheck_program ?(init_state = empty_grcheck_state) (program : t_prog) =
  message "grchecking: ";
  pprint pprint_t_prog program;

  let rec loop (program : t_prog) =
    match program with
    | [] ->
      let* ctx = get_gr_ctx in
      let* ctx2 = get_ty_ctx in
      return ([], VSCtx.empty, ctx, ctx2)
    | d :: ds -> (grcheck_decl d) (loop ds)

  in loop program init_state
