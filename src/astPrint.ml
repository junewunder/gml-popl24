open Ast
open Format

let verbose = ref false

let message s =
  if (!verbose) then fprintf std_formatter "@.\n%s" s
let pprint f x =
  if (!verbose) then fprintf std_formatter "@.%a" f x

let pprint_loc f ((loc_start, loc_end) : loc) =
  if (!verbose) then
  fprintf f "%s:%i:%i-%i:%i"
    loc_start.pos_fname
    loc_start.pos_lnum
    (loc_start.pos_cnum - loc_start.pos_bol)
    loc_end.pos_lnum
    (loc_end.pos_cnum - loc_end.pos_bol)

let rec seq p sep f l =
  match l with
  | [] -> ()
  | [x] -> fprintf f "@[%a@]" p x
  | x::r -> fprintf f "@[%a@]@,%s@[%a@]@,"
              p x
              sep
              (seq p sep) r

let pair p1 p2 sep f (x, y) =
  fprintf f "%a@[%s%a@]"
  p1 x
  sep
  p2 y

let rec pprint_vs_typ u f vst = match vst with
| VSTVertex -> fprintf f "◆"
| VSTVar x -> fprintf f "%s" x
| VSTUVar i -> fprintf f "%a" u i
| VSTProd [] -> fprintf f "[]"
| VSTProd (vst :: []) -> fprintf f "[ %a ]" (pprint_vs_typ u) vst
| VSTProd vsts -> fprintf f "[%a]" (seq (pprint_vs_typ u) " × ") vsts
| VSTCoRec (x, vst) -> fprintf f "(corec %s. %a)" x (pprint_vs_typ u) vst

let rec pprint_vs (e, v) f vs =
  match vs with
  | VSVar u -> v f u
  | VSUVar a -> fprintf f "%a" e a
  | VSTuple vss ->
     fprintf f "@[<1>(@[%a@])@]"
       (seq (pprint_vs (e, v)) ", ") vss
  | VSProj (vs, i, _) ->
     fprintf f "@[%a.%d@]"
       (pprint_vs (e, v)) vs
       i

let rec pprint_graph (e, v, vt) f g =
  let ppg = pprint_graph (e, v, vt) in
  match g with
  | GEmpty -> fprintf f "•"
  | GVar gv -> fprintf f "%s" gv
  | GSeq (g1, g2) ->
     fprintf f "@[<1>(@[%a@]@ + @[%a@])@]"
       ppg g1
       ppg g2
  | GPar (g1, g2) ->
     fprintf f "@[<1>(@[%a@]@ * @[%a@])@]"
       ppg g1
       ppg g2
  | GOr (g1, g2) ->
     fprintf f "@[<1>(@[%a@]@ \\/ @[%a@])@]"
       ppg g1
       ppg g2
  | GFut (GEmpty, u) ->
     fprintf f "@[<1>@[%a@]/%a@]"
       ppg GEmpty
       (pprint_vs (e, v)) u
  | GFut (g, u) ->
     fprintf f "@[<1>(@[%a@])/%a@]"
       ppg g
       (pprint_vs (e, v)) u
  | GTouch u -> fprintf f "@[%a->@]" (pprint_vs (e, v)) u
  | GPi (uf, uft, ut, utt, g) ->
     fprintf f "@[<2>(pi %a : %a,@ %a : %a.@ @[<1>%a@])@]"
       v uf
       vt uft
       v ut
       vt utt
       ppg g
  | GRec (gv, g) ->
     fprintf f "@[<2>(rec %s.@ @[<1>%a@])@]"
       gv
       ppg g
  | GNew (u, ut, g) ->
     fprintf f "@[<2>(new %a : %a.@ @[<1>%a@])@]"
       v u
       vt ut
       ppg g
  | GApp (g, uf, ut) ->
     fprintf f "@[(%a)@,[%a,@ %a]@]"
       ppg g
       (pprint_vs (e, v)) uf
       (pprint_vs (e, v)) ut
  | GUVar v -> e f v

let rec pprint_typ (e1, e2, g, vv, v, vp) f t =
  let ppt = pprint_typ (e1, e2, g, vv, v, vp) in
  match t.tdesc with
  | TVar v -> fprintf f "%s" v
  | TRef t -> fprintf f "@[%a@]@ ref" ppt t
  | TArrow ({tdesc = TProd []; _}, t2, vp1, vp2, _) ->
    fprintf f "@[<1>pi %a,@ %a.@ @[%a@]@]"
      vp vp1
      vp vp2
      ppt t2
  | TArrow (t1, t2, vp1, vp2, gr) ->
    fprintf f "@[<1>pi %a,@ %a.@ (@[%a@]@ @[<2>--%a-->@]@ @[%a@])@]"
      vp vp1
      vp vp2
      ppt t1
      g gr
      ppt t2
  | TProd ts ->
     seq ppt " * " f ts
  | TFuture (u, t) ->
     fprintf f "@[<1>@[%a@]@ future[%a]@]"
       ppt t
       v u
  | TUVar v -> fprintf f "%a" e1 v
  | TRecType _ -> fprintf f "TRecType"
  | TConstr (tname, ts, u) ->
      (
        match ts with
        | [] -> ()
        | t :: [] -> fprintf f "%a " ppt t
        | ts -> fprintf f "(%a) " (seq ppt ", ") ts
      );
      fprintf f "%s" (string_of_longid tname);
      fprintf f "[%a]" v u (* TODO I WOULD REALLY LIKE THIS TO NOT PRINT ANYTHING IF u IS EMPTY*)

let rec pprint_schema (e1, e2, g, vv, v, vp) f s =
  match s with
  | SMono t -> pprint_typ (e1, e2, g, vv, v, vp) f t
  | SForall (a, s) | SForallG (a, s) ->
     fprintf f "%s. @[%a@]" a (pprint_schema (e1, e2, g, vv, v, vp)) s

let string_of_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Eq -> "="
  | Ne -> "<>"
  | And -> "&&"
  | Or -> "||"
  | Concat -> "^"

let rec pprint_match_case (vv, v, vp, vt, t) f (c, xs, e) =
  fprintf f "| %s (%a) -> @[%a@]"
    (string_of_longid c)
    (seq pp_print_string ", ") xs
    (pprint_expr (vv, v, vp, vt, t)) e

and pprint_expr (vv, v, vp, vt, t) f e =
  let ppe = pprint_expr (vv, v, vp, vt, t) in
  match e.edesc with
  | EVar v -> fprintf f "%s" (string_of_longid v)
  | EConst (Num n) -> fprintf f "%d" n
  | EConst (String s) -> fprintf f "%s" s
  | EConst (Char c) -> fprintf f "%c" c
  | EConst (Bool true) -> fprintf f "true"
  | EConst (Bool false) -> fprintf f "false"
  | EConst Unit -> fprintf f "()"
  | EConst (Futref vs) -> fprintf f "futref[@[%a@]]"
                            v vs
  | EInfixop (o, e1, e2) -> fprintf f "@[<1>@[%a@]@ %s @[%a@]@]"
                              ppe e1
                              (string_of_op o)
                              ppe e2
  | EFunc (is_rec, fname, xname, _, _, uf, ut, ebody) ->
    fprintf f "@[<2>fun %s@,[@[%a@],@ @[%a@]]@ %s ->@ @[%a@]@]"
      (if String.length fname > 0 then fname ^ " " else "") (* anonymous functions have name `""` *)
      vp uf
      vp ut
      xname
      ppe ebody
  | EIf (e1, e2, e3) ->
     fprintf f "@[<2>if@ @[%a@]@ then@ @[%a@]@ else@ @[%a@]@]"
       ppe e1
       ppe e2
       ppe e3
  | ELet (x, _, e1, e2) ->
     fprintf f "@[<2>let %s@ : %a@ =@ @[%a@]@ in@ @[%a@]@]"
       x
       t e1.etyp
       ppe e1
       ppe e2
  | ELetTuple (xs, e1, e2) ->
     fprintf f "@[<2>let (%a) =@ @[%a@]@ in@ @[%a@]@]"
       (seq (fun f s -> fprintf f "%s" s) ", ") xs
       ppe e1
       ppe e2
  | ELetRecord (fields, e1, e2) ->
     fprintf f "@[<2>let {%a} =@ @[%a@]@ in@ @[%a@]@]"
       (seq (fun f (field, v) -> fprintf f "%s = %s"
                                   (string_of_longid field)
                                   v) ", ") fields
       ppe e1
       ppe e2
  | EApp (e1, uf, ut, e2) ->
     fprintf f "@[<1>@[(%a)@,[@[%a@],@ @[%a@]]@]@ @[(%a)@]@]"
       ppe e1
       v uf
       v ut
       ppe e2
  | ETuple (es) ->
     fprintf f "@[(%a)@]"
       (seq ppe ", ") es
  | ERef e -> fprintf f "@[<1>ref@ @[%a@]@]" ppe e
  | EDeref e -> fprintf f "@[<1>!(@[%a@])@]" ppe e
  | EUpdate (e1, e2) ->
     fprintf f "@[<1>@[%a@]@ := @[%a@]@]"
       ppe e1
       ppe e2
  | EFuture (u, e) -> fprintf f "@[<1>future@,[%a]@ @[%a@]@]"
                        v u
                        ppe e
  | EForce e -> fprintf f "@[<1>force@ @[%a@]@]" ppe e
  | EPar (e1, e2) -> fprintf f "@[<2>par@ (@[%a@],@ @[%a@])@]"
                       ppe e1
                       ppe e2
  | EAnnot (e, _) -> ppe f e
  | ENewVert (u, ut, e) ->
     fprintf f "@[<2>new @[%a : %a@].@ @[%a@]@]"
       vv u
       vt ut
       ppe e
  | EMatch (scrutinee, cases) ->
    fprintf f "@[match %a with@]@.@[%a@]"
      ppe scrutinee
      (seq (pprint_match_case (vv, v, vp, vt, t)) "") cases
  | ETry (_, _) -> fprintf f "print unimplemented"


let rec pprint_decl (vv, v, vp, vt, t) f d =
  let ppe = pprint_expr (vv, v, vp, vt, t) in
  match d.ddesc with
  | DVal (x, _, e) ->
     fprintf f "@[<1>let@ %s:@ @[%a@] =@ @[%a@]@]"
       x
       t e.etyp
       ppe e
  | DExp e -> ppe f e
  | _ -> ()

let pprint_void _ a = abort a

let pprint_ignore f _ = fprintf f "_"

let pprint_uvar f i = fprintf f "$%i" i

let pprint_vertex_var f (VId x) =
  let ppx = UnionFind.pp (fun f -> Format.fprintf f "%s") in
  fprintf f "%s" (UnionFind.keyof x)

let pprint_g_vs_typ = pprint_vs_typ pprint_uvar

let pprint_c_vs_typ = pprint_vs_typ pprint_void

let pprint_vertex_param (f : formatter) ((v, vt) : vertex_var * g_vs_typ) : unit =
  fprintf f "%a : %a" pprint_vertex_var v pprint_g_vs_typ vt

let pprint_c_vertex_param (f : formatter) ((v, vt) : vertex_var * c_vs_typ) : unit =
  fprintf f "%a : %a" pprint_vertex_var v pprint_c_vs_typ vt

let pprint_g_vs : formatter -> g_vertex_struct -> unit =
  pprint_vs (pprint_uvar, pprint_vertex_var)

let pprint_c_vs : formatter -> c_vertex_struct -> unit =
  pprint_vs (pprint_void, pprint_vertex_var)

let pprint_g_graph : formatter -> g_graph -> unit =
  pprint_graph (pprint_uvar, pprint_vertex_var, pprint_g_vs_typ)

let pprint_c_graph : formatter -> c_graph -> unit =
  pprint_graph (pprint_void, pprint_vertex_var, pprint_c_vs_typ)

let pprint_p_typ : formatter -> p_typ -> unit =
  pprint_typ (pprint_ignore, pprint_ignore, pprint_ignore, pprint_ignore, pprint_ignore, pprint_ignore)

let pprint_t_typ : formatter -> t_typ -> unit =
  pprint_typ (pprint_uvar, pprint_ignore, pprint_ignore, pprint_ignore, pprint_ignore, pprint_ignore)

let pprint_t_schema : formatter -> t_schema -> unit =
  pprint_schema (pprint_uvar, pprint_ignore, pprint_ignore, pprint_ignore, pprint_ignore, pprint_ignore)

let pprint_g_typ : formatter -> g_typ -> unit =
  pprint_typ (pprint_void, pprint_uvar, pprint_g_graph, pprint_vertex_var, pprint_g_vs, pprint_vertex_param)

let pprint_c_typ : formatter -> c_typ -> unit =
  pprint_typ (pprint_void, pprint_ignore, pprint_c_graph, pprint_vertex_var, pprint_c_vs, pprint_c_vertex_param)

let pprint_g_schema : formatter -> g_schema -> unit =
  pprint_schema (pprint_void, pprint_uvar, pprint_g_graph, pprint_vertex_var, pprint_g_vs, pprint_vertex_param)

let pprint_c_schema : formatter -> c_schema -> unit =
  pprint_schema (pprint_void, pprint_ignore, pprint_c_graph, pprint_vertex_var, pprint_c_vs, pprint_c_vertex_param)

let pprint_p_expr : formatter -> p_expr -> unit =
  pprint_expr
    ( pprint_ignore
    , pprint_ignore
    , pprint_ignore
    , pprint_ignore
    , pprint_ignore
    )

let pprint_t_expr : formatter -> t_expr -> unit =
  pprint_expr
    ( pprint_ignore
    , pprint_ignore
    , pprint_ignore
    , pprint_ignore
    , pprint_t_typ
    )

let pprint_g_expr : formatter -> g_expr -> unit =
  pprint_expr
    ( pprint_vertex_var
    , pprint_g_vs
    , pprint_vertex_param
    , pprint_g_vs_typ
    , pprint_g_typ
    )

let pprint_p_decl : formatter -> p_decl -> unit =
  pprint_decl
    ( pprint_ignore
    , pprint_ignore
    , pprint_ignore
    , pprint_ignore
    , pprint_ignore
    )

let pprint_t_decl : formatter -> t_decl -> unit =
  pprint_decl
    ( pprint_ignore
    , pprint_ignore
    , pprint_ignore
    , pprint_ignore
    , pprint_t_typ
    )

let pprint_g_decl : formatter -> g_decl -> unit =
  pprint_decl
    ( pprint_vertex_var
    , pprint_g_vs
    , pprint_vertex_param
    , pprint_g_vs_typ
    , pprint_g_typ
    )

let pprint_t_prog = seq pprint_t_decl ";;\n\n"
let pprint_g_prog = seq pprint_g_decl ";;\n\n"

let pprint_str_bindings pp f =
  fprintf f "@.";
  seq (pair Format.pp_print_string pp " = ") "\n" f

let pprint_vvar_bindings pp f =
  fprintf f "@.";
  seq (pair pprint_vertex_var pp " = ") "\n" f

let pprint_int_bindings pp f =
  fprintf f "@.";
  seq (pair Format.pp_print_int pp " = ") "\n" f

let pprint_errorable f (e : errorable) =
  match e with
| PDecl x -> fprintf f "@,PDecl %a" pprint_p_decl x
| TDecl x -> fprintf f "@,TDecl %a" pprint_t_decl x
| PExpr x -> fprintf f "@,PExpr %a" pprint_p_expr x
| TExpr x -> fprintf f "@,TExpr %a" pprint_t_expr x
| GExpr x -> fprintf f "@,GExpr %a" pprint_g_expr x
| PType x -> fprintf f "@,PType %a" pprint_p_typ x
| TType x -> fprintf f "@,TType %a" pprint_t_typ x
| GType x -> fprintf f "@,GType %a" pprint_g_typ x
| TSchema x -> fprintf f "@,TSchema %a" pprint_t_schema x
| Graph x -> fprintf f "@,Graph %a" pprint_g_graph x
| VS x -> fprintf f "@,VS %a" pprint_g_vs x
| VST x -> fprintf f "@,VST %a" pprint_g_vs_typ x
| UnifyTTys (x, y) ->
    fprintf f "@,unifying types:@.  @[%a@]@.  @[%a@]"
      pprint_t_typ x
      pprint_t_typ y
| UnifyGTys (x, y) ->
    fprintf f "@,unifying types:@.  @[%a@]@.  @[%a@]"
      pprint_g_typ x
      pprint_g_typ y
| UnifyGrs (x, y) ->
    fprintf f "@,unifying graphs:@.  @[%a@]@.  @[%a@]"
      pprint_g_graph x
      pprint_g_graph y
| UnifyVSinGrs (x, y) ->
    fprintf f "@,unifying vertex structures in the graphs:@.  @[%a@]@.  @[%a@]"
      pprint_g_graph x
      pprint_g_graph y
| UnifyVSandGrInTys (x, y) ->
    fprintf f "@,unifying vertex structures and graphs in the types:@.  @[%a@]@.  @[%a@]"
      pprint_g_typ x
      pprint_g_typ y
| UnifyVSs (x, y) ->
    fprintf f "@,unifying vertex structures:@.  @[%a@]@.  @[%a@]"
      pprint_g_vs x
      pprint_g_vs y


(* pprint_vertex_var, pprint_g_vs, pprint_vertex_param, pprint_g_vs_typ, pprint_g_typ *)

(* let pprint_vert f s = Format.fprintf f "%s" (Vertex.classname s) *)

(* let pprint_cgraph f g =
  let g = Graph.simplify ~recs:false g in
  pprint_graph (noneformatter, pprint_vert) f g *)

(* let pprint_t_type : Format.formatter -> t_typ -> unit  =
  pprint_type (noneformatter, pprint_cgraph, pprint_vert, noneformatter) *)

(* let pprint_annot_schema : Format.formatter -> annot_schema -> unit =
  pprint_schema (noneformatter, pprint_cgraph, pprint_vert, _) *)

(* let pprint_annot_expr f e =
  pprint_expr
    (noneformatter, pprint_t_type, pprint_cgraph, pprint_vert)
    f
    e *)

(* let pprint_annot_decl f d =
  pprint_decl
    (noneformatter, pprint_t_type, pprint_cgraph, pprint_vert)
    f
    d *)

(* let pprint_annot_prog f =
  seq pprint_annot_decl "\n\n" f *)
