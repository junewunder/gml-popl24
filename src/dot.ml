open Ast

type dotvert = string * (string option)

type edgetype = Touch | Spawn | Thread
type edge = { src  : string;
              dest : string;
              typ  : edgetype }
             
type dotgraph = { id        : string;
                  label     : string option;
                  vertices  : dotvert list;
                  edges     : edge list;
                  subgs     : t list;
                  source    : string;
                  sink      : string;
                }
and t = Dotgraph of dotgraph

let print_vvar (VId k) = UnionFind.findkey k
let rec print_vs vs =
  match vs with
  | VSVar v -> print_vvar v
  | VSUVar _ -> ""
  | VSTuple vss ->
     "(" ^ (String.concat ", " (List.map print_vs vss)) ^ ")"
  | VSProj (vs, i, _) ->
     (print_vs vs) ^ "•" ^ (string_of_int i)

let v_ctr = ref 0
let new_v () =
  (v_ctr := !v_ctr + 1;
   "v" ^ (string_of_int (!v_ctr)))

let c_ctr = ref 0
let new_cluster () =
  (v_ctr := !v_ctr + 1;
   "cluster" ^ (string_of_int (!v_ctr)))

let rec sub v v' g =
  let vs =
    List.fold_left
      (fun vs (oldv, oldl) ->
        if oldv = v then (v', oldl)::vs
        else (oldv, oldl)::vs)
      []
      g.vertices
  in
  let subgs =
    List.fold_left
      (fun gs (Dotgraph g) ->
        let g' = sub v v' g in
        (Dotgraph g')::gs)
      []
      g.subgs
  in
  { id = g.id;
    label = None;
    vertices = vs;
    edges = List.map
              (fun e ->
                if e.dest = v then {e with dest = v'}
                else if e.src = v then {e with src = v'}
                else e)
              g.edges;
    subgs = subgs;
    source = if g.source = v then v' else g.source;
    sink = if g.sink = v then v' else g.sink
  }

let rename_g g =
  List.fold_left
    (fun g v ->
      let v' = new_v () in
      sub (fst v) v' g)
    g
    g.vertices



let rec of_graph_int g =
  let mk_sub id g =
    {id = "";
     label = None;
     vertices = [];
     edges = [];
     subgs = [Dotgraph {g with id = new_cluster (); label = id}];
     source = g.source;
     sink = g.sink;
    }
  in
  let singleton v label =
     {id = ""; label = None; vertices = [(v, label)]; edges = [];
      subgs = []; source = v; sink = v}
  in
  let seq g1 g2 =
    {id = "";
     label = None;
     vertices = g1.vertices @ g2.vertices;
     edges = {src = g1.sink; dest = g2.source; typ = Thread}
             ::(g1.edges @ g2.edges);
     subgs = g1.subgs @ g2.subgs;
     source = g1.source;
     sink = g2.sink
    }
  in
  let or_gr g1 g2 =
    let g2 = rename_g g2 in
    {id = new_cluster ();
     label = Some "or";
     vertices = [];
     edges = [];
     subgs = [Dotgraph (mk_sub (Some "alt2") g2);
              Dotgraph (mk_sub (Some "alt1") g1)
             ];
     source = g1.source;
     sink = g2.sink
    }
  in
  let par g1 g2 =
    let s = new_v () in
    let t = new_v () in
    {id = "";
     label = None;
     vertices = [(s, None); (t, None)] @ (g1.vertices @ g2.vertices);
     edges = [{src = s; dest = g1.source; typ = Spawn};
              {src = s; dest =  g2.source; typ = Spawn};
              {src = g1.sink; dest = t; typ = Touch};
              {src = g2.sink; dest = t; typ = Touch}]
               @ g1.edges @ g2.edges;
     subgs = g1.subgs @ g2.subgs;
     source = s;
     sink = t
    }
  in
  let fut g v =
    (*
    let rec sub oldt v g =
  let (subbed1, vs) =
    List.fold_left
      (fun (subbed, vs) (oldv, oldl) ->
        if oldv = oldt && oldl = None then
          (true, (v, Some v)::vs)
        else (subbed, (oldv, oldl)::vs))
      (false, [])
      g.vertices
  in
  let (subbed2, subgs) =
    List.fold_left
      (fun (subbed, gs) (Dotgraph g) ->
        let (sg, g') = sub oldt v g in
        (subbed || sg, (Dotgraph g')::gs))
      (false, [])
      g.subgs
  in
  let subbed = subbed1 || subbed2 in
  (subbed,
   { id = g.id;
     label = None;
     vertices = vs;
     edges = List.map
               (fun (a, b) -> if b = oldt && subbed then (a, v) else (a, b))
               g.edges;
     subgs = subgs;
     source = if g.source = oldt && subbed then v else g.source;
     sink = if g.sink = oldt && subbed then v else oldt
  })
    in
     *)
    let v' = new_v () in
    let v = print_vs v in
    let oldt = g.sink in
    (*
    let (subbed, g) = sub oldt v g in
     *)
    let subbed = false in
    {id = "";
     label = None;
     vertices =
       (v', None)::(v, Some v)::g.vertices;
     edges = {src = v'; dest = g.source; typ = Spawn}
             ::{src = oldt; dest = v; typ = Thread}::g.edges;
     subgs = g.subgs;
     source = v';
     sink = v'}
  in
  let touch v =
    let v = print_vs v in
    let v' = new_v () in
    let g = singleton v' None in
    {g with edges = [{src = v; dest = v'; typ = Touch}]}
  in
  let rec getbinders ?(showouterpis=false) g =
    match g with
    (* | GPi ([], [], g) -> getbinders ~showouterpis:showouterpis g *)
    | GPi (uf, _, ut, _, g) when showouterpis ->
       let (s, g) = getbinders ~showouterpis:true g in
       (Printf.sprintf
          "Π %s, %s. %s"
          (print_vvar uf)
          (print_vvar ut)
          s,
        g)
    | GPi (_, _, _, _, g) -> getbinders ~showouterpis:showouterpis g
    | GRec (gv, g) ->
       let (s, g) = getbinders ~showouterpis:true g in
       (Printf.sprintf
          "μ %s. %s"
          gv
          s,
        g)
    (* | GNew ([], g) -> getbinders ~showouterpis:showouterpis g *)
    | GNew (u, _, g) when showouterpis ->
       let (s, g) = getbinders ~showouterpis:showouterpis g in
       (Printf.sprintf
          "ν %s. %s"
          (print_vvar u)
          s,
        g)
    | GNew (_, _, g) -> getbinders ~showouterpis:showouterpis g
    | _ -> ("", g)
  in
  match g with
  | GEmpty -> singleton (new_v ()) None
  | GVar v -> singleton (new_v ()) (Some (v ^ "..."))
  | GSeq (g1, g2) -> seq (of_graph_int g1) (of_graph_int g2)
  | GPar (g1, g2) -> par (of_graph_int g1) (of_graph_int g2)
  | GOr (g1, g2) -> mk_sub (Some "or") (or_gr (of_graph_int g1) (of_graph_int g2))
  (* [of_graph_int g1; of_graph_int g2] *)
  | GFut (g, v) -> fut (of_graph_int g) v
  | GTouch u -> touch u
  | GUVar e -> abort e
  | GApp (g, VSTuple [], VSTuple []) -> of_graph_int g
  | GApp (g, uf, ut) ->
     (match g with
      | GVar gv ->
         singleton (new_v ())
           (Some
              (Printf.sprintf
                 "%s[%s, %s]"
                 gv
                 (print_vs uf)
                 (print_vs ut)
              )
           )
      | _ -> mk_sub (Some "app") (of_graph_int g))
  | _ -> let (s, g) = getbinders ~showouterpis: true g in
         let g = of_graph_int g in
         if s = "" then g
         else mk_sub (Some s) g




let of_graph g = Dotgraph (of_graph_int g)

let write (f: string) (d: t) =
  let printnode (id, l) =
    Printf.sprintf "%s[label = \"%s\"; shape=%s; width=0.1]"
      id
      (match l with Some s -> s | None -> "")
      (match l with Some _ -> "plain" | None -> "circle")
  in
  let print_edge {src; dest; typ} =
    src ^ "->" ^ dest ^ (if typ = Touch then " [constraint=false]" else "")
  in
  let rec code_of_t sub (Dotgraph g) =
    Printf.sprintf
      "%s %s {\n
       %s
       %s\n
       %s\n
       %s\n
       }"
      (if sub then "subgraph" else "digraph")
      (g.id)
      (match g.label with None -> "" | Some l -> "label=\"" ^ l ^ "\"\n")
      (String.concat ";\n" (List.map
                              printnode
                              g.vertices))
      (String.concat ";\n "
         (List.map print_edge g.edges))
      (String.concat "\n" (List.map (code_of_t true) g.subgs))
  in
  let outch = open_out f in
  output_string outch (code_of_t false d);
  close_out outch
