(* open Vertex
module VMap = VMap *)

(* let pp_position (pos : Lexing.position) = "" *)
type loc = Lexing.position * Lexing.position
let dloc = (Lexing.dummy_pos, Lexing.dummy_pos)

let pp_loc f (lstart, lend) =
  let open Lexing in
  Format.fprintf f
    "%s:%d:%d-%s:%d:%d"
    lstart.pos_fname
    lstart.pos_lnum
    (lstart.pos_cnum - lstart.pos_bol)
    lend.pos_fname
    lend.pos_lnum
    (lend.pos_cnum - lend.pos_bol)

let pp_void _ _ = ()

type longid = Id of string
            | Modid of string * longid
  [@@deriving show, eq]

let rec string_of_longid id =
  match id with
  | Id v -> v
  | Modid (m, id') -> m ^ "." ^ (string_of_longid id')

module S =
  struct
    type t = longid
    let rec compare l1 l2 =
      String.compare (string_of_longid l1) (string_of_longid l2)
  end

module VMap = Map.Make(S)

exception Impossible
type void = Void of void
let equal_void _ _ = raise Impossible

let rec abort (Void v) : 'a = abort v

type vertex_var = VId of string UnionFind.t [@@deriving show, eq]

type 'g_unif vs_typ =
  VSTVertex
| VSTVar of string
| VSTUVar of 'g_unif
| VSTProd of 'g_unif vs_typ list
| VSTCoRec of string * 'g_unif vs_typ
[@@deriving show, eq, ord]

(*
type 'g_unif vs_schema =
  VSMono of 'g_unif vs_typ
| VSForall of string * 'g_unif vs_schema
[@@deriving show, eq]
 *)

type ('g_unif) vertex_struct =
  VSVar of vertex_var
| VSUVar of 'g_unif
| VSTuple of ('g_unif) vertex_struct list
(* Last component is the VS type of the first component *)
| VSProj of ('g_unif) vertex_struct * int * 'g_unif vs_typ
[@@deriving show, eq]

type 'g_unif graph =
  GEmpty
| GVar of string
| GUVar of 'g_unif
| GSeq of 'g_unif graph * 'g_unif graph
| GOr of 'g_unif graph * 'g_unif graph
| GPar of 'g_unif graph * 'g_unif graph
| GFut of 'g_unif graph * 'g_unif vertex_struct
| GTouch of 'g_unif vertex_struct
(* |~| uf: Uf; ut: Ut. G *)
| GPi of vertex_var * 'g_unif vs_typ * vertex_var * 'g_unif vs_typ * 'g_unif graph
| GRec of string * 'g_unif graph
| GNew of vertex_var * 'g_unif vs_typ * 'g_unif graph
| GApp of 'g_unif graph
          * 'g_unif vertex_struct
          * 'g_unif vertex_struct
[@@deriving show, eq]

type ('t_unif, 'graph, 'vert, 'vert_param) typ_desc =
| TVar of string
| TUVar of 't_unif
| TRef of ('t_unif, 'graph, 'vert, 'vert_param) typ
| TArrow of ('t_unif, 'graph, 'vert, 'vert_param) typ
          * ('t_unif, 'graph, 'vert, 'vert_param) typ
          * 'vert_param
          * 'vert_param
          * 'graph
| TProd of ('t_unif, 'graph, 'vert, 'vert_param) typ list
| TFuture of 'vert * ('t_unif, 'graph, 'vert, 'vert_param) typ
(* e.g. t List.t[U] *)
| TConstr of longid * ('t_unif, 'graph, 'vert, 'vert_param) typ list * 'vert
| TRecType of (string * ('t_unif, 'graph, 'vert, 'vert_param) typ) list
[@@deriving show, eq]

and ('t_unif, 'graph, 'vert, 'vert_param) typ =
  { tdesc : ('t_unif, 'graph, 'vert, 'vert_param) typ_desc;
    tloc  : loc [@equal fun _ _ -> true] }
    [@@deriving show, eq]

type ('t_unif, 'graph, 'vert, 'vert_param) schema =
  SMono of ('t_unif, 'graph, 'vert, 'vert_param) typ
| SForall of string * ('t_unif, 'graph, 'vert, 'vert_param) schema
| SForallG of string * ('t_unif, 'graph, 'vert, 'vert_param) schema
[@@deriving show, eq]

let rec type_of_schema s  =
  match s with
  | SMono t -> t
  | SForall (_, s) | SForallG (_, s) -> type_of_schema s

type infixop =
  Plus | Minus | Times | Div
  | Lt | Le | Gt | Ge | Eq | Ne
  | And | Or
  | Concat
  [@@deriving show, eq]

type p_typ = (void, unit, unit, unit) typ [@@deriving show, eq]
type p_schema = (void, unit, unit, unit) schema [@@deriving show, eq]

type 'vert const =
  Num of int
| String of string
| Char of char
| Bool of bool
| Unit
| Futref of 'vert
[@@deriving show, eq]

type p_const = unit const [@@deriving show, eq]

type is_recursive = Recursive | Terminal
[@@deriving show, eq]

(* e.g. | Some x -> e is ("Some", ["x"], e) *)
type ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) match_case =
  longid * string list * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
[@@deriving show, eq]


(* NOTE: 'vert_param = vertex_name * vs_typ in graphchecking step  *)
(* Expressions *)
and ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr_desc =
  | EVar of longid
  | EConst of 'vert const
  | EInfixop of infixop
        * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
        * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr

  (* (rec, f, x, annotated arg typ, annotated ret typ, uf, ut, body) *)
  | EFunc of is_recursive
          * string * string
          * p_typ option * p_typ option
          * 'vert_param * 'vert_param
          * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr

  (* if e1 then e2 else e3 *)
  | EIf of ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
         * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
         * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr

  (* let x (: t) = e1 in e2 *)
  | ELet of string
          * p_typ option
          * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
          * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr

  (* let (x1, ..., xn) = e1 in e2 *)
  | ELetTuple of string list * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
                 * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr

  (* let {f1 = x1; ..., f2 = x2} = e1 in e2 *)
  | ELetRecord of (longid * string) list * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
                 * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr

  | EApp of ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
            * 'vert
            * 'vert
            * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr

  (* match e with ... *)
  | EMatch of ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
              * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) match_case list

  | ETuple of ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr list
  | ERef of ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
  | EDeref of ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
  | EUpdate of ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
               * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
  | EFuture of 'vert * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
  | EForce of ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
  | EPar of ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
          * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
  | ETry of ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
            * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) match_case list

  (* Annotations (e : t) *)
  | EAnnot of ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr * p_typ
  | ENewVert of vertex_var * 'g_unif vs_typ * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
[@@deriving show, eq]

and ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr =
  { edesc  : ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr_desc;
    eloc   : loc [@equal fun _ _ -> true];
    etyp   : 'typ;
    egr    : 'graph
  }
[@@deriving show, eq]

type ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) decl_desc =
  (* let x (: t) = e;; *)
  | DVal of string * p_typ option * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
  (* let (rec?) f (x (:t1)) (:t2) = e1;; *)
(*
  | DFun of is_recursive * string * string * p_typ option * p_typ option
            * 'vert_param * 'vert_param
            * ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
            *)
  (* e;; *)
  | DExp of ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) expr
  | DExtType of string
  | DExtRecType of string * (longid * p_typ) list
  | DExternal of longid * p_typ
  (* type ('unif, 'b) t = | A of int * string | ...*)
  | DTypeDef of string list * string * (string * p_typ) list
[@@deriving show, eq]

and ('t_unif, 'g_unif, 'typ, 'schema, 'graph, 'vert, 'vert_param) decl =
  { ddesc : ('t_unif, 'g_unif, 'typ, 'graph, 'vert, 'vert_param) decl_desc;
    dloc  : loc [@equal fun _ _ -> true];
    dinfo : 'schema }
[@@deriving show, eq]

(* A program is a list of declarations *)
type ('t_unif, 'g_unif, 'typ, 'schema, 'graph, 'vert, 'vert_param) prog =
  ('t_unif, 'g_unif, 'typ, 'schema, 'graph, 'vert, 'vert_param) decl list
[@@deriving show, eq]


(* expr, decl, programs immediately after parsing *)
type p_expr = (void, void, unit, unit, unit, unit) expr [@@deriving show, eq]
type p_decl = (void, void, unit, unit, unit, unit, unit) decl [@@deriving show, eq]
type p_prog = (void, void, unit, unit, unit, unit, unit) prog [@@deriving show, eq]

(* expr, decl, programs that have been type annotated *)
type t_typ = (int, unit, unit, unit) typ [@@deriving show, eq]
type t_schema = (int, unit, unit, unit) schema [@@deriving show, eq]
type t_expr = (int, void, t_typ, unit, unit, unit) expr [@@deriving show, eq]
type t_decl = (int, void, t_typ, t_schema, unit, unit, unit) decl [@@deriving show, eq]
type t_prog = (int, void, t_typ, t_schema, unit, unit, unit) prog [@@deriving show, eq]

(* expr, decl, programs that have been type and graph annotated *)
(* g_graph is a "graphing graph" a graph in the process of being inferred *)

type g_vs_typ = int vs_typ [@@deriving show, eq]
(* type g_vs_schema = int vs_schema [@@deriving show, eq] *)
type g_vertex_struct = int vertex_struct [@@deriving show, eq]
type g_graph = int graph [@@deriving show, eq]
type g_typ = (void, g_graph, g_vertex_struct, vertex_var * g_vs_typ) typ [@@deriving show, eq]
type g_schema = (void, g_graph, g_vertex_struct, vertex_var * g_vs_typ) schema [@@deriving show, eq]
type g_expr = (void, int, g_typ, g_graph, g_vertex_struct, vertex_var * g_vs_typ) expr [@@deriving show, eq]
type g_decl = (void, int, g_typ, g_schema, g_graph, g_vertex_struct, vertex_var * g_vs_typ) decl [@@deriving show, eq]
type g_prog = (void, int, g_typ, g_schema, g_graph, g_vertex_struct, vertex_var * g_vs_typ) prog [@@deriving show, eq]

(* expr, decl, programs that have been type and graph annotated *)
(* c_graph is a "concrete graph" a fully inferred graph with no unification variables *)
type c_vs_typ = void vs_typ [@@deriving show, eq]
type c_vertex_struct = void vertex_struct [@@deriving show, eq]
type c_graph = void graph [@@deriving show, eq]
type c_typ = (void, c_graph, void vertex_struct, vertex_var * c_vs_typ) typ [@@deriving show, eq]
type c_schema = (void, c_graph, void vertex_struct, vertex_var * c_vs_typ) schema [@@deriving show, eq]
type c_expr = (void, void, c_typ, c_graph, void vertex_struct, vertex_var * c_vs_typ) expr [@@deriving show, eq]
type c_decl = (void, void, c_typ, c_schema, c_graph, void vertex_struct, vertex_var * c_vs_typ) decl [@@deriving show, eq]
type c_prog = (void, void, c_typ, c_schema, c_graph, void vertex_struct, vertex_var * c_vs_typ) prog [@@deriving show, eq]

let rec extract_ty_from_schema ty_s =
  match ty_s with
  | SMono t -> t
  | SForall (_, t) -> extract_ty_from_schema t
  | SForallG (_, t) -> extract_ty_from_schema t

let base_ty_of_string s : t_typ =
  let typ ty =
  { tdesc = ty
  ; tloc = dloc
  }
  in
  typ (TConstr (Id s, [], ()))

type errorable =
  | PDecl of p_decl
  | TDecl of t_decl
  | PExpr of p_expr
  | TExpr of t_expr
  | GExpr of g_expr
  | PType of p_typ
  | TType of t_typ
  | GType of g_typ
  | TSchema of t_schema
  | Graph of g_graph
  | VS of g_vertex_struct
  | VST of g_vs_typ
  | UnifyTTys of t_typ * t_typ
  | UnifyGTys of g_typ * g_typ
  | UnifyGrs of g_graph * g_graph
  | UnifyVSinGrs of g_graph * g_graph
  | UnifyVSandGrInTys of g_typ * g_typ
  | UnifyVSs of g_vertex_struct * g_vertex_struct
  [@@deriving show]

let get_file_loc (stack : errorable list) =
  let f sum e = match sum, e with
  | Some x, _ -> Some x
  | None, PExpr e -> Some e.eloc
  | None, TExpr e -> Some e.eloc
  | None, GExpr e -> Some e.eloc
  | _ -> None
  in
  List.fold_left f None stack
