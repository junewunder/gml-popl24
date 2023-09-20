open Ast
open Graph
open Visitor
open AstPrint

exception UnboundGraphVariable of string

let rec substitute_vert_var (e : c_vertex_struct) (x : vertex_var) (replacement : c_vertex_struct) =
  match e with
  | VSVar y when x = y -> replacement
  | VSVar y -> VSVar y
  | VSUVar i -> VSUVar i
  | VSTuple es -> VSTuple (List.map (fun e -> substitute_vert_var e x replacement) es)
  | VSProj (e, i, n) -> VSProj (substitute_vert_var e x replacement, i, n)

let substitute_vert_var_in_gr (gr : c_graph) (name : vertex_var) (vs : c_vertex_struct) : c_graph =
  transform_graph id (fun v -> substitute_vert_var v name vs) id gr

let rec size g =
  match g with
  | GEmpty -> 0
  | GVar _ | GUVar _ | GTouch _ -> 1
  | GSeq (g1, g2) | GPar (g1, g2) -> 1 + (size g1) + (size g2)
  | GOr (g1, g2) -> 1 + (max (size g1) (size g2))
  | GFut (g, _)
    | GPi (_, _, _, _, g)
    | GNew (_, _, g)
    | GApp (g, _, _) -> 1 + (size g)
  | GRec (_, g) -> 10

let rec vs_size vs =
  match vs with
  | VSVar _ | VSUVar _ -> 1
  | VSTuple vss -> List.fold_left (+) 0 (List.map vs_size vss)
  | VSProj (vs, _, _) -> 1 + vs_size vs
          

let rec real_size g =
  match g with
  | GEmpty -> 1
  | GVar _ | GUVar _ -> 1
  | GTouch vs -> vs_size vs
  | GSeq (g1, g2) | GPar (g1, g2) | GOr (g1, g2)
    -> 1 + (real_size g1) + (real_size g2)
  | GFut (g, vs) -> 1 + vs_size vs
  | GPi (_, _, _, _, g)
    | GNew (_, _, g) -> 1 + (real_size g)
  | GApp (g, vs1, vs2) -> 1 + real_size g + vs_size vs1 + vs_size vs2
  | GRec (_, g) -> 1 + real_size g

let rec addone g =
  match g with
  | GTouch _ -> GSeq (g, GEmpty)
  | GFut (g, u) -> GFut (addone g, u)
  | GPi (uf, uft, ut, utt, g) -> GPi (uf, uft, ut, utt, addone g)
  | GNew (u, ut, g) -> GNew (u, ut, addone g)
  | GApp (g, uf, ut) -> GApp (addone g, uf, ut)
  | _ -> GSeq (GEmpty, g)

let next_norm_var_id = ref 0

let new_norm_var () =
  let id = !next_norm_var_id in
  next_norm_var_id := id + 1;
  VSVar (VId (UnionFind.newkey ("n" ^ (string_of_int id))))

let rec expand_max e (n: int) (g: c_graph) : c_graph =
  let g' = 
  if n <= 0 then GVar ""
  else
    match g with
    | GEmpty -> GEmpty
    | GVar v ->
       (match VMap.find_opt (Id v) e with
        | Some g -> (* mk_graph (GSeq (mk_graph GEmpty, expand_max e n g)) *)
           addone (expand_max e n g)
        | None -> GVar v)
    | GSeq (g1, g2) ->
       GSeq (expand_max e n g1, expand_max e n g2)
    | GPar (g1, g2) -> mk_par (expand_max e n g1) (expand_max e n g2)
    | GOr (g1, g2) ->
       let g1 = expand_max e n g1 in
       let g2 = expand_max e n g2 in
       (* expand_max e n *)
         (if size g1 < size g2 then g2 else g1)
(*
           if n = 0 then g1 else g2
          else
            if n = 0 then g2 else g1)
 *)
    | GFut (g, u) -> mk_fut (expand_max e n g) u
    | GTouch u -> mk_touch u
    | GPi (uf, uft, ut, utt, g) -> mk_pi uf uft ut utt (expand_max e n g)
    | GRec (gv, g0) ->
       expand_max (VMap.add (Id gv) g e) (n - 1) g0
    | GNew (u, ut, g) ->
       let u' = new_norm_var () in
       substitute_vert_var_in_gr (expand_max e n g) u u'
    | GApp (g, uf, ut) ->
       let g = expand_max e n g in
       (match g with
        | GPi (uf0, _, ut0, _, g0) ->
           (* If the head of the function graph is a pi, just do the
            * beta reduction *)
           let g' = substitute_vert_var_in_gr g0 uf0 uf in
           substitute_vert_var_in_gr g' ut0 ut
        | _ -> g)
    | GUVar a -> GUVar a
  in
  g'

               
let expand_max n g = expand_max (VMap.empty) n g

let rec pairwise f l1 l2 =
  List.concat (List.map (fun x -> List.map (fun y -> f x y) l2) l1)

(*
let rec norm_once e (g: c_graph) : c_graph list =
  (*
  let _ = Format.fprintf Format.std_formatter "Norming %a\n"
            Print.pprint_cgraph g in
  let _ = Format.print_newline () in
   *)
  match g with
  | GEmpty -> [GEmpty]
  | GVar v ->
     (match VMap.find_opt v e with
      | None -> [mk_graph (GVar v)]
      | Some g -> [g])
  | GSeq (g1, g2) -> pairwise mk_seq (norm_once e g1) (norm_once e g2)
  | GOr (g1, g2) -> (norm_once e g1) @ (norm_once e g2)
  | GPar (g1, g2) -> pairwise mk_par (norm_once e g1) (norm_once e g2)
  | GFut (g, u) -> List.map (fun g -> mk_fut g u) (norm_once e g)
  | GTouch u -> [mk_touch u]
  | GPi (uf, ut, g) -> List.map (mk_pi uf ut) (norm_once e g)
  | GRec (gv, g) -> norm_once (VMap.add gv (mk_graph GEmpty) e) g
  | GNew (us, g) ->
     let us' = List.map
                 (fun _ -> vert_of_string false (new_global_vertex ())) us
     in
     List.map (fun g -> sub_all gvsub g us us') (norm_once e g)
  | GApp (g, uf, ut) ->
     (match gdesc g with
      | GPi (uf0, ut0, g) ->
         List.map (fun g0 -> sub_all gvsub g0 (uf0 @ ut0) (uf @ ut))
           (norm_once e g)
      | GRec (gv, g0) ->
         (match gdesc g0 with
          | GPi (uf0, ut0, g0) ->
             List.map (fun g0 -> sub_all gvsub g0 (uf0 @ ut0) (uf @ ut))
               (norm_once (VMap.add gv (mk_graph GEmpty) e) g0)
          | _ -> raise (Invalid_argument "norm_once"))
      | _ -> List.map
               (fun g ->
                 match gdesc g with
                 | GEmpty -> mk_graph GEmpty
                 | _ -> mk_graph (GApp (g, uf, ut)))
               (norm_once e g))
  | GUVar a -> [mk_graph (GUVar a)]

let norm_once g = norm_once VMap.empty g

type normgraph =
  { vertices : string list;
    edges    : string list VMap.t;
    source   : string;
    sink     : string;
  }

let rec norm_of_dot (Dot.Dotgraph g: Dot.t) : normgraph =
  let subgs = List.map norm_of_dot g.Dot.subgs in
  let subvs = List.concat (List.map (fun g -> g.vertices) subgs) in
  let gedges =
    List.fold_left
      (fun emap (s, t) ->
        match VMap.find_opt s emap with
        | None -> VMap.add s [t] emap
        | Some ts -> VMap.add s (t::ts) emap)
      VMap.empty
      g.Dot.edges
  in
  let edges = List.fold_left
                (VMap.merge
                    (fun s ts1 ts2 ->
                      match (ts1, ts2) with
                      | (None, _) -> ts2
                      | (_, None) -> ts1
                      | (Some ts1, Some ts2) -> Some (ts1 @ ts2)))
                gedges
                (List.map (fun g -> g.edges) subgs)
  in
  { vertices = (List.map fst g.Dot.vertices) @ subvs;
    edges    = edges;
    source   = g.Dot.source;
    sink     = g.Dot.sink;
  }

type topo = InProgress | Done

let nbrs g n =
  match VMap.find_opt n g.edges with
  | Some l -> l
  | None -> []

exception Cyclic
let toposort (g: normgraph) : string list option =
  let m : (topo VMap.t) ref = ref VMap.empty in
  let sorted : string list ref = ref [] in
  let rec ts n =
    match VMap.find_opt n !m with
    | Some Done -> ()
    | Some InProgress -> raise Cyclic
    | None ->
       (m := VMap.add n InProgress !m;
        let nbrs = nbrs g n in
        List.iter ts nbrs;
        (*
        ctr := !ctr + 1;
        m := VMap.add n (Done !ctr) !m
         *)
        sorted := n::!sorted;
        m := VMap.add n Done !m)
  in
  try
    ts g.source;
    Some !sorted
     (*
    Some (VMap.map (function InProgress -> failwith "toposort"
                           | Done n -> n)
            !m) *)
  with Cyclic -> None
 *)
