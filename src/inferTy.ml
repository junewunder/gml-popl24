open Ast
(* open Graph
open Vertex *)
open Visitor
open AstPrint

(* TODO: make Ctx's use longid's instead of strings *)
module Ctx = Map.Make (String)

module Subst = Map.Make (Int)

type ty_ctx = t_schema Ctx.t
type tyname_ctx = (string list * t_schema) Ctx.t

type subst = t_typ Subst.t

(* let pp_ty_ctx (fmt : Format.formatter) (ctx : ty_ctx) =
  Format.fprintf fmt "%a" [%show: (string * ty_schema) list] (Ctx.bindings ctx) *)
let show_ty_ctx (ctx : ty_ctx) = [%show: (string * t_schema) list] (Ctx.bindings ctx)
let show_subst (s : subst) = [%show: (int * t_typ) list] (Subst.bindings s)

(* let pp ty_ *)
type tycheck_state = {
  next_unif_var_num : int ref;
  ty_ctx : ty_ctx [@opaque];
  tyname_ctx : ty_ctx [@opaque]; (* Maps type aliases *)
  (* Maps constructor names to a type schema of a TArrow (arg type, ret type) *)
  con_ctx : ty_ctx [@opaque];
  inspection_stack : errorable list;
} [@@deriving show]

type ty_error_desc =
  | TyErrUnboundVar of string
  | TyErrCannotUnify of t_typ * t_typ
  | TyErrUnimplemented of string
  | TyErrUnknown of string
  | TyErrInvalidPattern
  | TyErrCantGeneralize
  | TyErrCantInstantiateWithFuture
  [@@deriving show]

type ty_error =
  { tedesc : ty_error_desc
  ; teloc : loc option
  ; teinspection_stack : errorable list
  } [@@deriving show]

let ty_err_no_loc e = { tedesc = e; teloc = None; teinspection_stack = []}

let ty_err e loc = { tedesc = e; teloc = Some loc; teinspection_stack = []}

exception TypeError of ty_error [@@deriving show]

type 'a tycheck_result = ('a, ty_error) result [@@deriving show]

type 'a tychecker = tycheck_state -> 'a tycheck_result

let (>>=) (m : 'a tychecker) (f : 'a -> 'b tychecker) : 'b tychecker =
  fun state ->
    match m state with
    | Ok res -> f res state
    | Error err -> Error err

let (>>) m f = m >>= fun _ -> f

let (let*) = (>>=)

let return (x : 'a) : 'a tychecker = fun _ -> Ok x

let fail err = fun st ->
  Error
  { tedesc = err
  ; teloc = None
  ; teinspection_stack = st.inspection_stack
  }

let fail_with_loc err loc = fun st ->
  Error
  { tedesc = err
  ; teloc = Some loc
  ; teinspection_stack = st.inspection_stack
  }

let rec all (ms : 'a tychecker list) : 'a list tychecker =
    match ms with
    | [] -> return []
    | m :: ms ->
      let* m = m in
      let* ms = all ms in
      return (m :: ms)

let foldM f a bs = List.fold_left (fun a b -> let* a = a in f a b) (return a) bs

let rec mapM f l =
  match l with
  | [] -> return []
  | a::l -> let* a = f a in
            let* l = mapM f l in
            return (a::l)

let get_state : tycheck_state tychecker =
  fun state -> Ok state

let get_ctx : ty_ctx tychecker =
  fun state -> Ok state.ty_ctx

let get_tyname_ctx : ty_ctx tychecker =
  fun state -> Ok state.ty_ctx

let get_con_ctx : ty_ctx tychecker =
  fun state -> Ok state.con_ctx

let with_transformed_state f m : 'a tychecker=
  fun state -> m (f state)

let with_transformed_ty_ctx (f : ty_ctx -> ty_ctx) m =
  with_transformed_state (fun state -> { state with ty_ctx = f state.ty_ctx }) m

let with_extended_ty_ctx name ty (m : 'a tychecker) : 'a tychecker =
  with_transformed_ty_ctx (Ctx.add name ty) m

let rec with_extended_ty_ctx_n names ts (m : 'a tychecker) : 'a tychecker =
  match names, ts with
  | [], [] -> m
  | name :: names, t :: ts ->
    with_extended_ty_ctx name t begin
      with_extended_ty_ctx_n names ts m
    end
  | _ -> fail TyErrInvalidPattern

let new_unif_var : int tychecker = fun state ->
  state.next_unif_var_num := 1 + !(state.next_unif_var_num);
  return (!(state.next_unif_var_num)) state

let rewrap_typ prev next_ty =
  { prev with
    tdesc = next_ty
  }
let typ ty =
  { tdesc = ty
  ; tloc = dloc
  }

let inspect (x : errorable) m : 'a tychecker =
  let transform state =
    {state with inspection_stack = x :: state.inspection_stack}
  in
  with_transformed_state transform m

let rec substitute (s : subst) (ty : t_typ) : t_typ =
  (* perform subst of all unification variables *)
  let wrap = rewrap_typ ty in
  match ty.tdesc with
  | TUVar i -> (
      try substitute s (Subst.find i s)
      with Not_found -> wrap @@ TUVar i
    )
  | TVar x -> wrap @@ TVar x
  | TArrow (t1, t2, _, _, _) -> wrap @@ TArrow (substitute s t1, substitute s t2, (), (), ())
  | TFuture ((), inner) -> wrap @@ TFuture ((), substitute s inner)
  | TRef t -> wrap @@ TRef (substitute s t)
  | TProd ts ->
     wrap @@ (TProd (List.map (substitute s) ts))
  | TConstr (id, ts, _) ->
     wrap @@ (TConstr (id, List.map (substitute s) ts, ()))
  | TRecType _ -> raise @@ TypeError (ty_err (TyErrUnimplemented "1") ty.tloc)

let rec substitute_ty_schema (s: subst) (ty_schema : t_schema) : t_schema =
  match ty_schema with
  | SMono t -> SMono (substitute s t)
  | SForall (x, t) -> SForall (x, substitute_ty_schema s t)
  | SForallG  (x, t) -> SForallG (x, substitute_ty_schema s t)

let substitute_ty_in_expr (s : subst) (e : t_expr) : t_expr =
  transform_expr id id (substitute s) id id id id e

let substitute_env (s : subst) (ty_ctx : ty_ctx) : ty_ctx =
  Ctx.map (substitute_ty_schema s) ty_ctx

let with_substituted_ty_ctx subst (m : 'a tychecker) : 'a tychecker =
  with_transformed_ty_ctx (substitute_env subst) m

(* prioritizes s1 over s2. applies s1 to s2 *)
let overwrite_subst (s1 : subst) (s2 : subst) : subst =
  let merge _ x y = match x, y with
    | Some x, _ -> Some x
    | None, Some y -> Some (substitute s1 y)
    | None, None -> None
  in Subst.merge merge s1 s2

let rec contains_unif_var (i : int) (t : t_typ) : bool =
  match t.tdesc with
  | TVar _ -> false
  | TUVar j -> i = j
  | TRef t
    | TFuture (_, t) -> contains_unif_var i t
  | TArrow (t1, t2, _, _, _) -> (contains_unif_var i t1) || (contains_unif_var i t2)
  | TProd ts -> List.fold_left (fun sum t -> contains_unif_var i t || sum) false ts
  | TConstr (_, ts, _) -> List.exists (contains_unif_var i) ts
  | TRecType cons -> List.exists (fun (_, t) -> contains_unif_var i t) cons

let rec get_ty_params t = match t.tdesc with
  | TVar x -> [x]
  | TUVar _ -> []
  | TRef t -> get_ty_params t
  | TArrow (i, o, _, _, _) -> get_ty_params i @ get_ty_params o
  | TProd ts -> List.concat_map get_ty_params ts
  | TFuture (_, t) -> get_ty_params t
  | TConstr (_, ts, _) -> List.concat_map get_ty_params ts
  | TRecType xs -> List.concat_map (fun (x, t) -> get_ty_params t) xs

let build_ty_schema (t : t_typ) (var_names : string list) : t_schema =
  List.fold_right (fun i t -> SForall (i, t)) var_names (SMono t)

(** only generalizes unification variables that are free in t but not free in the environment.
    this is important in the case `let x = y in x` *)
let generalize (t : t_typ) : (t_schema * subst) tychecker =
  inspect (TType t) begin
    let rec extract_unif_vars (t : t_typ) : int list =
      match t.tdesc with
      | TVar _ -> []
      | TUVar i -> [i]
      | TArrow (t1, t2, _, _, _) -> (extract_unif_vars t1) @ (extract_unif_vars t2)
      | TRef t
      | TFuture (_, t) -> extract_unif_vars t
      | TProd ts
      | TConstr (_, ts, _) -> List.concat (List.map extract_unif_vars ts)
      | TRecType _ -> raise @@ TypeError (ty_err_no_loc (TyErrUnimplemented "3"))
    in
    let extract_ctx_unif_vars ty_ctx : int list =
      let ctx_unif_vars = Ctx.map (extract_unif_vars) (Ctx.map extract_ty_from_schema ty_ctx) in
      (Ctx.bindings ctx_unif_vars) |> List.map snd |> List.concat |> List.sort_uniq Int.compare
    in
    let var_name_of_int i =
      "'" ^ String.make 1 @@ Char.chr (97 + i)
    in
    let* ty_ctx = get_ctx in
    let free_unif_ctx_vars = extract_ctx_unif_vars ty_ctx in
    let unif_vars = List.sort_uniq Int.compare (extract_unif_vars t) in
    let unif_vars = List.filter (fun i -> not @@ List.exists ((=) i) free_unif_ctx_vars) unif_vars in
    let unif_vars_from_zero = List.init (List.length unif_vars) (fun x -> x) in
    let var_names = List.map var_name_of_int unif_vars_from_zero in
    let vars = List.map (fun x -> typ @@ TVar x) var_names in
    let new_unif_var_names = (Subst.of_seq @@ List.to_seq (List.combine unif_vars vars)) in
    let generalized_ty = substitute new_unif_var_names t in
    return (build_ty_schema generalized_ty var_names, new_unif_var_names)
  end

let rec unify (t1 : t_typ) (t2 : t_typ) : subst tychecker =
  inspect (UnifyTTys (t1, t2)) begin
    let fail_cannot_unify = fail @@ TyErrCannotUnify (t1, t2) in
    match t1.tdesc, t2.tdesc with
    | TVar x1, TVar x2 ->
      if x1 = x2
        then return Subst.empty
        else fail_cannot_unify
    | TUVar i1, TUVar i2 when i1 = i2 -> return Subst.empty
    | TUVar i, t
    | t, TUVar i ->
      if contains_unif_var i (typ t)
        then fail_cannot_unify
        else return (Subst.singleton i @@ typ t)
    | (TArrow (t1_arg, t1_body, _, _, _)), (TArrow (t2_arg, t2_body, _, _, _)) ->
      let* arg_subst = unify t1_arg t2_arg in
      let* body_subst = unify (substitute arg_subst t1_body) (substitute arg_subst t2_body) in
      unify_substs arg_subst body_subst
    | TRef (inner1), TRef (inner2)
    | TFuture (_, inner1), TFuture (_, inner2) ->
      unify inner1 inner2
    | TProd ts1, TProd ts2 ->
      if List.length ts1 <> List.length ts2 then fail_cannot_unify else
      let* substs = all @@ List.map (fun (t1, t2) -> unify t1 t2) (List.combine ts1 ts2) in
      let* subst = unify_substs_many substs in
      return subst
    | TConstr (id1, ts1, _), TConstr (id2, ts2, _) ->
      if string_of_longid id1 <> string_of_longid id2 then fail_cannot_unify
      else
        if List.length ts1 <> List.length ts2 then fail_cannot_unify else
      let* substs = all @@ List.map (fun (t1, t2) -> unify t1 t2) (List.combine ts1 ts2) in
      let* subst = unify_substs_many substs in
      return subst
    | _ -> fail_cannot_unify
  end

and unify_substs (s1 : subst) (s2 : subst) : subst tychecker =
  let unifs1 = List.map fst (Subst.bindings s1) in
  let unifs2 = List.map fst (Subst.bindings s2) in
  let unifs = List.sort_uniq Int.compare (unifs1 @ unifs2) in

  let get_entry i =
    match Subst.find_opt i s1, Subst.find_opt i s2 with
    | Some x, Some y ->
      let* s = unify x y in
      let x = substitute s x in
      let y = substitute s y in
      if not (equal_t_typ x y) then fail (TyErrCannotUnify (x, y)) else
      return (x, s)
    | Some x, None
    | None, Some x -> return (x, Subst.empty)
    | None, None -> fail (TyErrUnknown "unify_substs")
  in
  let f (sum : subst) i =
    let* (x, s) = (get_entry i) in
    if not (Subst.is_empty s)
      then unify_substs (Subst.add i x sum) s
      else return (Subst.add i x sum)
  in
  foldM f Subst.empty unifs

and unify_substs_many (ss : subst list) : subst tychecker =
  foldM unify_substs Subst.empty ss

let rec inst (t : t_schema) : t_typ tychecker =
  inspect (TSchema t) begin
    let rec inst_ty (x : string) (i : int) (t : t_typ) =
      rewrap_typ t @@ match t.tdesc with
      | TVar y -> if x = y then TUVar i else TVar y
      | TUVar i -> TUVar i
      | TRef t -> TRef (inst_ty x i t)
      | TArrow (t1, t2, a, b, g) -> TArrow (inst_ty x i t1, inst_ty x i t2, a, b, g)
      | TProd ts -> TProd (List.map (inst_ty x i) ts)
      | TFuture (_, t) -> TFuture ((), inst_ty x i t)
      | TConstr (id, ts, _) -> TConstr (id, List.map (inst_ty x i) ts, ())
      | TRecType cons ->
        TRecType (List.map (fun (conid, t) -> (conid, inst_ty x i t)) cons)
    in match t with
    | SMono t_inner -> return t_inner
    | SForall (x, t) ->
      let* unif_idx = new_unif_var in
      let* t' = inst t in (
        try return @@ inst_ty x unif_idx t'
        with TypeError e -> fail e.tedesc
      )
    | SForallG (x, t) -> raise @@ TypeError (ty_err_no_loc (TyErrUnimplemented "5"))
  end

let trust_p_typ (t : p_typ) : t_schema =
  let id = fun _ -> () in
  let err = fun _ -> raise @@ TypeError (ty_err_no_loc (TyErrUnknown "1")) in
  (SMono (transform_typ err id id id t))

let rec tycheck (e : p_expr) : (t_typ * subst * t_expr) tychecker =
  inspect (PExpr e) begin
    let* (ty, subst, e) = match e.edesc with
      | EVar x -> tycheck_var x e
      | EConst c -> tycheck_const c e
      | EApp (lam, _, _, arg) -> tycheck_application lam arg e
      | ELet (binder, bound_ty_sugg, bound, body) -> tycheck_let binder bound bound_ty_sugg body e
      | EFuture ((), body) -> tycheck_future body e
      | ETuple es -> tycheck_tuple es e
      | EPar (left, right) -> tycheck_par left right e
      | EForce body -> tycheck_force body e
      | EIf (cond, then_branch, else_branch) ->
        tycheck_if cond then_branch else_branch e
      | EFunc (is_rec, f, x, annot1, annot2, (), (), body) ->
          tycheck_func is_rec f x annot1 annot2 body e
      | ELetTuple (binders, bound, body) -> tycheck_let_tuple binders bound body e
      | ERef body -> tycheck_ref body e
      | EDeref body -> tycheck_deref body e
      | EUpdate (e_ref, e_val) -> tycheck_update e_ref e_val e
      | EInfixop (op, e_operand1, e_operand2) ->
        tycheck_infixop op e_operand1 e_operand2 e
      | EMatch (cond, cases) -> tycheck_match cond cases e

      | _ -> fail (TyErrUnimplemented "6")
    in

    return (ty, subst, e)
  end

and rebuild e edesc ty : t_expr =
  { e with edesc; etyp = ty}

and tycheck_var x e =
  let* ty_ctx = get_ctx in
  let* x_ty_schema =
    try return @@ Ctx.find (string_of_longid x) ty_ctx
    with Not_found -> fail @@ TyErrUnboundVar (string_of_longid x)
  in
  let* x_ty = inst x_ty_schema in
  let e' = rebuild e (EVar x) x_ty in
  return (x_ty, Subst.empty, e')

and tycheck_const c e =
  let* ty = (
    match c with
      | Num _ -> return (base_ty_of_string "int")
      | String _ -> return (base_ty_of_string "string")
      | Char _ -> return (base_ty_of_string "char")
      | Bool _ -> return (base_ty_of_string "bool")
      | Unit -> return (base_ty_of_string "unit")
      | Futref _ ->
         let* unif_idx = new_unif_var in
         return (typ @@ TRef (typ (TFuture ((), typ @@ TUVar unif_idx))))
  ) in
  return (ty , Subst.empty, rebuild e (EConst c) ty)

and tycheck_application lam arg e =
  let* (lam_ty, lam_subst, lam_annot) = tycheck lam in
  let* (arg_ty, arg_subst, arg_annot) = with_substituted_ty_ctx lam_subst (tycheck arg) in
  let* unif_idx = new_unif_var in
  let* unif_subst = unify (substitute arg_subst lam_ty) @@ typ (TArrow (arg_ty, typ (TUVar unif_idx), (), (), ())) in
  let ret_ty = substitute unif_subst @@ typ (TUVar unif_idx) in
  let* subst = unify_substs_many [unif_subst; arg_subst; lam_subst] in
  let lam_annot = substitute_ty_in_expr subst lam_annot in
  let arg_annot = substitute_ty_in_expr subst arg_annot in
  return (
    ret_ty,
    subst,
    rebuild e (EApp (lam_annot, (), (), arg_annot)) ret_ty
  )

and tycheck_let binder bound bound_ty_sugg body e =
  let* (bound_ty, bound_subst, bound_annot) = tycheck bound in
  let* user_annot_subst = match bound_ty_sugg with
    | Some bound_ty_sugg ->
      let* bound_ty_sugg = inst (trust_p_typ bound_ty_sugg) in
      unify bound_ty_sugg bound_ty
    | None -> return Subst.empty
  in
  let* bound_subst = unify_substs user_annot_subst bound_subst in
  let bound_ty = substitute user_annot_subst bound_ty in

  let* (body_ty, body_subst, body_annot, unif_subst) = with_substituted_ty_ctx bound_subst begin
    let* bound_ty_schema, unif_subst = generalize (substitute bound_subst bound_ty) in
    let* (a, b, c) = with_extended_ty_ctx binder bound_ty_schema (tycheck body) in
    return (a, b, c, unif_subst)
  end in
  let* subst = unify_substs_many [body_subst; bound_subst; unif_subst] in
  let bound_annot = substitute_ty_in_expr subst bound_annot in
  let body_annot = substitute_ty_in_expr subst body_annot in
  return (
    body_ty,
    subst,
    rebuild e (ELet (binder, bound_ty_sugg, bound_annot, body_annot)) body_ty
    )

and tycheck_let_tuple binders bound body e =
  let* (bound_ty, bound_subst, bound_annot) = tycheck bound in
  let* tys =
    mapM (fun _ -> let* v = new_unif_var in return @@ typ @@ TUVar v) binders
  in
  let* subst = unify bound_ty (typ (TProd tys)) in
  let* bound_subst = unify_substs subst bound_subst in
  let rec do_substs binders bound_tys =
    match (binders, bound_tys) with
    | ([], []) -> let* x = tycheck body in return (x, Subst.empty)
    | (binder::binders, bound_ty::bound_tys) ->
        let* bound_ty_schema, unif_subst = generalize (substitute bound_subst bound_ty) in
        let* x, sum = with_extended_ty_ctx binder bound_ty_schema
          (do_substs binders bound_tys) in
        let* unif_subst = unify_substs sum unif_subst in
        return (x, unif_subst)
    | _ -> fail TyErrInvalidPattern
  in
  let* ((body_ty, body_subst, body_annot), sum_subst) =
    with_substituted_ty_ctx bound_subst (do_substs binders tys)
  in
  let* subst = unify_substs_many [sum_subst; body_subst; bound_subst] in
  let body_ty = substitute subst body_ty in
  let e =
    rebuild e (ELetTuple (binders, bound_annot, body_annot)) body_ty
  in
  let e = substitute_ty_in_expr subst e in
  return (
    body_ty,
    subst,
    e
    )

and tycheck_func is_rec f x annot1 annot2 body e =
  let* unif_idx = new_unif_var in
  let x_ty = TUVar unif_idx in
  let* unif_idx_f = new_unif_var in
  let f_ty = TUVar unif_idx_f in
  let* (fun_body_ty, fun_body_subst, fun_body_t) =
    match is_rec with
    | Recursive ->
      with_extended_ty_ctx x (SMono (typ x_ty)) begin
        with_extended_ty_ctx f (SMono (typ f_ty)) begin
          tycheck body
        end
      end
    | Terminal -> with_extended_ty_ctx x (SMono (typ x_ty)) (tycheck body)
  in
  let* arg_annot_subst = match annot1 with
    | Some arg_ty_sugg ->
      let* arg_ty_sugg = inst (trust_p_typ arg_ty_sugg) in
      unify arg_ty_sugg (typ x_ty)
    | None -> return Subst.empty
  in
  let x_ty = substitute arg_annot_subst (typ x_ty) in
  let* ret_annot_subst = match annot2 with
    | Some ret_ty_sugg ->
      let* ret_ty_sugg = inst (trust_p_typ ret_ty_sugg) in
      unify ret_ty_sugg fun_body_ty
    | None -> return Subst.empty
  in
  let* body_subst = unify_substs ret_annot_subst fun_body_subst in
  let body_ty = substitute ret_annot_subst fun_body_ty in
  let* ret_unify_subst = unify (typ @@ TArrow (x_ty, body_ty, (), (), ())) (typ f_ty) in
  let* subst = unify_substs_many [arg_annot_subst; ret_unify_subst; ret_annot_subst; body_subst] in
  let x_ty_substituted = substitute subst x_ty in
  let ret_ty_substituted = substitute subst body_ty in
  let expr_ty = typ @@ TArrow (x_ty_substituted, ret_ty_substituted, (), (), ()) in
  let e' = rebuild e
      (EFunc (is_rec, f, x, annot1, annot2, (), (), fun_body_t))
      expr_ty
  in
  let e' = substitute_ty_in_expr subst e' in
  return (
    expr_ty,
    subst,
    e'
  )

and tycheck_future body e =
  let* (body_ty, body_subst, body_annot) = tycheck body in
  return (
    typ @@ TFuture ((), body_ty),
    body_subst,
    rebuild e (EFuture ((), body_annot)) (typ @@ TFuture ((), body_ty))
    )

and tycheck_ref body e =
  let* (body_ty, body_subst, body_annot) = tycheck body in
  return (
    typ @@ TRef body_ty,
    body_subst,
    rebuild e (ERef body_annot) (typ @@ TRef body_ty)
  )

and tycheck_tuple es e =
  let tycheckers = List.map (fun e -> tycheck e) es in
  let* results = all tycheckers in
  let ts = List.map (fun (t, _, _) -> t) results in
  let substs = List.map (fun (_, s, _) -> s) results in
  let es_annot = List.map (fun (_, _, e) -> e) results in
  let* subst = unify_substs_many substs in
  let e' = rebuild e (ETuple es_annot) (typ (TProd ts)) in
  let e' = substitute_ty_in_expr subst e' in
  return ((typ @@ TProd ts), subst, e')

and tycheck_par left right e =
  let* (left_ty, left_subst, left_annot) = tycheck left in
  let* (right_ty, right_subst, right_annot) = tycheck right in
  let* subst = unify_substs left_subst right_subst in
  let t' = (typ @@ TProd [left_ty; right_ty]) in
  let e' = rebuild e (EPar (left_annot, right_annot)) t' in
  let e' = substitute_ty_in_expr subst e' in
  return (t', subst, e')

and tycheck_force body e =
  let* unif_idx = new_unif_var in
  let* (body_ty, body_subst, body_annot) = tycheck body in
  let* unif_subst = unify (typ @@ TFuture((), typ @@ TUVar unif_idx)) body_ty in
  let* subst = unify_substs unif_subst body_subst in
  let body_annot = substitute_ty_in_expr subst body_annot in
  let t' = substitute subst (typ @@ TUVar unif_idx) in
  let e' = rebuild e (EForce body_annot) t' in
  let e' = substitute_ty_in_expr subst e' in
  return (t', subst, e')

and tycheck_deref body e =
  let* unif_idx = new_unif_var in
  let* (body_ty, body_subst, body_annot) = tycheck body in
  let* unif_subst = unify (typ @@ TRef (typ @@ TUVar unif_idx)) body_ty in
  let* subst = unify_substs unif_subst body_subst in
  let body_annot = substitute_ty_in_expr subst body_annot in
  let t' = substitute subst (typ @@ TUVar unif_idx) in
  let e' = rebuild e (EDeref body_annot) t' in
  let e' = substitute_ty_in_expr subst e' in
  return (t', subst, e')

and tycheck_update e_ref e_val e =
  let* unif_idx = new_unif_var in
  let* (ref_ty, ref_subst, ref_annot) = tycheck e_ref in
  let* unif_subst = unify (typ @@ TRef (typ @@ TUVar unif_idx)) ref_ty in
  let* subst = unify_substs unif_subst ref_subst in
  let rhs_ty = substitute subst (typ @@ TUVar unif_idx) in
  let* (val_ty, val_subst, val_annot) =
    with_substituted_ty_ctx subst (tycheck e_val)
  in
  let t_subst = substitute val_subst rhs_ty in
  let* inner_ty_subst = unify rhs_ty val_ty in
  let* subst = unify_substs_many [subst; val_subst; inner_ty_subst] in
  let e' = rebuild e (EUpdate (ref_annot, val_annot)) (base_ty_of_string "unit") in
  let e' = substitute_ty_in_expr subst e' in
  return ((base_ty_of_string "unit"), subst, e')

and tycheck_if cond then_case else_case e =
  let* (cond_ty, cond_subst, cond_annot) = tycheck cond in
  let* unif_subst = unify (base_ty_of_string "bool") cond_ty in
  let* subst = unify_substs unif_subst cond_subst in
  let* (then_ty, then_subst, then_annot) =
    with_substituted_ty_ctx subst (tycheck then_case)
  in
  let* (else_ty, else_subst, else_annot) =
    with_substituted_ty_ctx subst (tycheck else_case)
  in
  let* branch_subst = unify then_ty else_ty in
  let* subst = unify_substs_many [subst; then_subst; else_subst; branch_subst]
  in
  let e' = rebuild e (EIf (cond_annot, then_annot, else_annot)) else_ty in
  let e' = substitute_ty_in_expr subst e' in
  return (else_ty, subst, e')

and tycheck_infixop op e_operand1 e_operand2 e =
  let bts = base_ty_of_string in
  let (expected_t1, expected_t2, return_ty) =
    match op with
    | Plus | Minus | Times | Div -> (bts "int", bts "int", bts "int")
    | Lt | Le | Gt | Ge | Eq | Ne -> (bts "int", bts "int", bts "bool")
    | And | Or -> (bts "bool", bts "bool", bts "bool")
    | Concat -> (bts "string", bts "string", bts "string")
  in
  let* (operand1_ty, operand1_subst, operand1_annot) = tycheck e_operand1 in
  let* (operand2_ty, operand2_subst, operand2_annot) = tycheck e_operand2 in
  let* unif_subst1 = unify expected_t1 operand1_ty in
  let* unif_subst2 = unify expected_t2 operand2_ty in
  let* subst = unify_substs_many
                 [operand1_subst; operand2_subst; unif_subst1; unif_subst2]
  in
  let operand1_annot = substitute_ty_in_expr subst operand1_annot in
  let operand2_annot = substitute_ty_in_expr subst operand2_annot in
  let e' = rebuild e (EInfixop (op, operand1_annot, operand2_annot))
             return_ty
  in
  let e' = substitute_ty_in_expr subst e' in
  return (return_ty, subst, e')

and [@warning "-8"] [@warning "-9"] [@warning "-5"] tycheck_match cond cases e =
  let* (cond_ty, cond_subst, cond_annot) = tycheck cond in
  let* con_ctx = get_con_ctx in

  pprint pprint_t_typ cond_ty;
  pprint pprint_t_expr cond_annot;

  let check_case (constructor, vars, case_body) =
    (* TODO: we are missing typechecking information here that should be passed through to grcheck *)
    let schema = Ctx.find (string_of_longid constructor) con_ctx in
    message ("constructor schema " ^ string_of_longid constructor);
    pprint pprint_t_schema schema;
    let* con_schema = inst schema in
    let {tdesc = TArrow (vars_tys, con_ret, _, _, _)} = con_schema in
    let* cond_unif_subst = unify con_ret cond_ty in

    let vars_tys = match vars_tys.tdesc with
      | TProd vars_tys -> vars_tys
      | _ -> [vars_tys]
    in

    let vars_tys = List.map (fun x -> SMono x) vars_tys in
    let* (case_ty, case_subst, case_annot) = with_extended_ty_ctx_n vars vars_tys begin
      tycheck case_body
    end in

    let* subst = unify_substs case_subst cond_unif_subst in
    let case_ty = substitute case_subst case_ty in
    let case_annot = substitute_ty_in_expr case_subst case_annot in

    return (case_ty, subst, case_annot)
  in

  let* outputs = mapM check_case cases in
  let case_tys    = List.map (fun (x, _, _) -> x) outputs in
  let case_substs = List.map (fun (_, x, _) -> x) outputs in
  let cases_annot = List.map (fun (_, _, x) -> x) outputs in

  let return_ty :: return_tys = List.map (fun e -> e.etyp) cases_annot in
  let* (_, cases_subst) = foldM (fun (prev_ty, sum_subst) ty ->
    let* subst = unify ty prev_ty in
    let* sum_subst = unify_substs subst sum_subst in
    return (ty, sum_subst)
  ) (return_ty, Subst.empty) return_tys in

  let* subst = unify_substs_many @@ [cases_subst; cond_subst] @ case_substs in
  let return_ty = substitute subst return_ty in

  let cond_ty = substitute subst cond_ty in

  message "cond_ty";
  pprint pprint_t_typ cond_ty;

  let cases = List.map2 (fun (x, y, _) z -> (x, y, z)) cases cases_annot in
  let e = rebuild e (EMatch (cond_annot, cases)) return_ty in
  let e = substitute_ty_in_expr subst e in

  return (return_ty, subst, e)

let add_rec_ty_to_ctx
      name
      (tyvars: string list)
      (constructors: (string * t_typ) list)
      (tyname_ctx, con_ctx, tyctx) =
  let ty_alias =
    typ @@ TConstr (Id name, List.map (fun x -> typ @@ TVar x) tyvars, ())
  in
  let mk_schema t =
    List.fold_left (fun t a -> SForall (a, t)) (SMono t) tyvars
  in
  let open AstPrint in
  pprint (fun f () ->
      Format.fprintf f "Adding %d constructors for %s"
        (List.length constructors)
        name
    )
    ();
  let add_const con_ctx (conid, argty) =

    let s = mk_schema (typ (TArrow (argty, ty_alias, (), (), ()))) in
    pprint (fun f () -> Format.fprintf f "%s: %a" conid pprint_t_schema s) ();
    Ctx.add conid s con_ctx

  in
  (Ctx.add name (mk_schema (typ (TRecType constructors))) tyname_ctx,
   List.fold_left add_const con_ctx constructors,
   List.fold_left add_const tyctx constructors
  )

let (tyname_ctx, con_ctx, tyctx) =
  List.fold_left (fun d (a, b, c) -> add_rec_ty_to_ctx a b c d)
    (Ctx.empty, Ctx.empty, Ctx.empty)
    [("option", ["'a"],
      [("None", typ @@ TProd []);
       ("Some", typ @@ TVar "'a")]);
     ("list", ["'a"],
      [("Nil", typ @@ TProd []);
       ("Cons", typ @@ TProd [typ @@ TVar "'a";
                              typ @@ TConstr (Id "list", [typ @@ TVar "'a"], ())])])
    ]

let empty_tycheck_state =
  { next_unif_var_num = ref 1
  ; ty_ctx = tyctx
  ; tyname_ctx = tyname_ctx
  ; con_ctx = con_ctx
  ; inspection_stack = []
  }

let rec contains_future t =
  match t.tdesc with
  | TVar _ | TUVar _ -> false
  | TRef t -> contains_future t
  | TArrow (t1, t2, _, _, _) -> contains_future t1 || contains_future t2
  | TProd ts | TConstr (_, ts, _) -> List.exists contains_future ts
  | TFuture _ -> true
  | TRecType cons ->
     List.exists (fun (_, t) -> contains_future t) cons

let rec check_instantiations loc t =
  match t.tdesc with
  | TVar _ -> return ()
  | TUVar _ -> fail_with_loc TyErrCantGeneralize loc
  | TRef t
    | TFuture (_, t) -> check_instantiations loc t
  | TArrow (t1, t2, _, _, _) ->
     let* _ = check_instantiations loc t1 in
     check_instantiations loc t2
  | TProd ts ->
     let* _ = mapM (check_instantiations loc) ts
     in return ()
  | TConstr (_, ts, _) ->
     if List.exists contains_future ts then
       fail_with_loc TyErrCantInstantiateWithFuture loc
     else
       let* _ = mapM (check_instantiations loc) ts
       in return ()
  | TRecType cons ->
     let* _ = mapM (fun (_, t) -> check_instantiations loc t) cons
     in return ()

let run_tycheck init_state e = (
    let* (ty, _subst, ty_annot_e) = tycheck e in
    let* state = get_state in
    let* ty_generalized, unif_subst = generalize ty in
    let ty_annot_e = substitute_ty_in_expr unif_subst ty_annot_e in
    return (ty_generalized, state, ty_annot_e)
  ) init_state

(* take a parsed type literal and "trust" it by turning it into a typechecker type
   this works because it parsed types will not contain any unification variables
   there should be no unification variables or graph type variables in a parsed program
   so we simply error in these cases
*)

let rec tycheck_decl (decl : p_decl) after =
  inspect (PDecl decl) begin
    let continue name ty_s decl_annot =
      let* (ctx, tyname_ctx, con_ctx, decls_annot) =
        with_extended_ty_ctx name ty_s after
      in
      return @@ (ctx, tyname_ctx, con_ctx, decl_annot :: decls_annot)
    in

    match decl.ddesc with
    | DVal (name, None, body) ->
    let* (ty, subst, body_annot) = tycheck body in
    let* dinfo, subst = generalize ty in
    let ty = substitute subst ty in
    let body_annot = substitute_ty_in_expr subst body_annot in
    let ddesc = (DVal (name, None, body_annot)) in
    let* _ = check_instantiations decl.dloc ty in
    with_substituted_ty_ctx subst begin
      continue name dinfo { decl with ddesc; dinfo }
      end

    | DVal (name, Some user_annot_ty, body) ->
    let* (ty, subst, body_annot) = tycheck body in
    let* uty = inst (trust_p_typ user_annot_ty) in
    let* subst' = unify ty uty in
    let* subst = unify_substs subst subst' in
    let ty = substitute subst ty in
    message "ty:";
    pprint pprint_t_typ ty;
    let* dinfo, subst'' = generalize ty in
    let* subst = unify_substs subst subst'' in
    let ty = substitute subst ty in
    let body_annot = substitute_ty_in_expr subst body_annot in
    let ddesc = (DVal (name, None, body_annot)) in
    let* _ = check_instantiations decl.dloc ty in
    with_substituted_ty_ctx subst begin
      continue name dinfo { decl with ddesc; dinfo }
    end

(*
  | DFun (is_rec, f, x, f_ty, x_ty, _, _, body) ->
      let* (ty, _, e_annot) = tycheck
        { edesc = EFunc (is_rec, f, x, f_ty, x_ty, (), (), body);
          egr = ();
          etyp = ();
          eloc = decl.dloc
        } in
      let* dinfo, subst = generalize ty in
      let ty = substitute subst ty in
      let body_annot = substitute_ty_in_expr subst body_annot in
      let ddesc = (DVal (name, None, body_annot)) in
      let* _ = check_instantiations decl.dloc ty in
      with_substituted_ty_ctx subst begin
        continue name dinfo { decl with ddesc; dinfo }
      end
 *)
    | DExp e ->
      tycheck_decl {decl with ddesc = DVal ("", None, e)} after

    | DTypeDef (tyvars, name, constructors) ->
      let* tyname_ctx = get_tyname_ctx in
      let* con_ctx = get_con_ctx in
      let* tyctx = get_ctx in
      let decl =
        { decl with ddesc = DTypeDef (tyvars, name, constructors);
                    dinfo = SMono (typ @@ TProd [])}
      in
      let constructors =
        List.map
          (fun (c, t) ->
            let [@warning "-8"] SMono t = trust_p_typ t in
            (c, t)
          )
          constructors
      in
      let (tyname_ctx, con_ctx, tyctx) =
        add_rec_ty_to_ctx name tyvars constructors
          (tyname_ctx, con_ctx, tyctx)
      in
      message "bindings in ty_ctx:";
      pprint (pprint_str_bindings pprint_t_schema)
        (Ctx.bindings tyctx);
      let* (ctx, tyname_ctx, con_ctx, decls_annot) =
        with_transformed_state
          (fun st -> { st with tyname_ctx = tyname_ctx;
                                con_ctx = con_ctx;
                                ty_ctx = tyctx})
          after
      in
      return (ctx, tyname_ctx, con_ctx, decl::decls_annot)

    | DExternal (x, pt) ->
      let tt = type_of_schema @@ trust_p_typ pt in
      let params = List.sort_uniq String.compare @@ get_ty_params tt in
      let tt = build_ty_schema tt params in
      let e = {decl with ddesc = DExternal (x, pt) ; dinfo = tt} in
      continue (string_of_longid x) tt e

    | DExtType _ -> after

    | _ -> fail (TyErrUnimplemented "tycheck_decl")
  end

let tycheck_program ?(init_state = empty_tycheck_state) (program : p_prog) :
      (ty_ctx * ty_ctx * ty_ctx * t_prog) tycheck_result =

  let rec loop (program : p_prog) =
    let* tyname_ctx = get_tyname_ctx in
    match program with
    | [] ->
       let* ctx = get_ctx in
       let* tyname_ctx = get_tyname_ctx in
       let* con_ctx = get_con_ctx in
       return (ctx, tyname_ctx, con_ctx, [])
    | d :: ds -> (tycheck_decl d) (loop ds)

  in loop program init_state
