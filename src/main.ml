open GrUtil
open Ast
open Lexing
open Visitor
open AstPrint
open GrPrint
open TyPrint

open InferTy
open InferGr

let usage_msg = "gml [OPTIONS] file"
let fname = ref ""
let timing = ref false
let ifile = ref ""
(* Analysis options *)
let fj = ref false
let deadlock = ref false
let ws = ref false
let vis = ref false
let vis_out = ref ""
let func = ref ""
let notypes = ref false
let print_sizes = ref false

let dl_example = ref None

(* Timers *)
let tinf_timer = ref 0.0
let ginf_timer = ref 0.0
let fj_timer = ref 0.0
let dl_timer = ref 0.0
let ws_timer = ref 0.0

let _ = message "\n"

(** Parse command line arguments **)
let do_vis f =
  vis := true;
  vis_out := f

let optspecs =
  [("--time", Arg.Set timing, "Output timing information");
   ("-t", Arg.Set timing, "Output timing information");
   ("-i", Arg.Set_string ifile, "File to output inferred types");
   ("--intf", Arg.Set_string ifile, "File to output inferred types");
   ("-s", Arg.Set print_sizes, "Print AST sizes of graph types");
   ("--sizes", Arg.Set print_sizes, "Print AST sizes of graph types");
   (*
   ("-fj", Arg.Set fj, "Convert to fork-join");
   ("--forkjoin", Arg.Set fj, "Convert to fork-join");
    *)
   ("-d", Arg.Set deadlock, "Run deadlock detection");
   ("--deadlock", Arg.Set deadlock, "Run deadlock detection");
   (*
   ("-ws", Arg.Set ws, "Run work-span analysis");
   ("--workspan", Arg.Set ws, "Run work-span analysis");
    *)
   ("-z", Arg.String do_vis, "Output DOT visualization");
   ("--dump-dot", Arg.String do_vis, "Output DOT visualization");
   ("-f", Arg.Set_string func, "Top-level binding to analyze; if unspecified, analyze whole program");
   ("--func", Arg.Set_string func, "Top-level binding to analyze; if unspecified, analyze whole program");
   ("-v", Arg.Set verbose, "Print debugging output");
   ("--verbose", Arg.Set verbose, "Print debugging output");
   ("-nt", Arg.Set notypes, "Supress output of type information");
   ("--no-types", Arg.Set notypes, "Supress output of type information");

   ("--dl-ex", Arg.Int (fun i -> dl_example := Some i),
    "Process hardcoded deadlock example #")
  ]

let infile_fun f =
  if !fname = "" then fname := f
  else
    raise (Arg.Bad "Wrong number of arguments.")

let _ = Arg.parse optspecs infile_fun usage_msg
let _ =
  if !fname = "" && !dl_example = None then
    (Arg.usage optspecs usage_msg;
     failwith "ERROR: no input program supplied.")


(* Run f arg and record the execution time in milliseconds in timer *)
let with_timer timer f arg =
  let start_t = Unix.gettimeofday () in
  let res = f arg in
  let end_t = Unix.gettimeofday () in
  let _ = timer := !timer +. ((end_t -. start_t) *. 1000.) in
  res

let log s = print_endline s
(* let log s =
  if !verbose then
    Printf.printf "%s\n%!" s
  else
    () *)

(* Parse a program from an input channel *)
let parse chan =
  let lexbuf = Lexing.from_channel chan in
  Parser.prog Lexer.token lexbuf

(* Run graph type inference on an AST *)
let infer_ty prog =
  with_timer tinf_timer InferTy.tycheck_program prog

let printnth inspection_stack n =
  try
    message ((string_of_int n) ^ ": ");
    pprint pprint_errorable (List.nth inspection_stack n)
  with Failure _ -> ()

(* Returns the graph associated a top-level declaration *)
let graph_of_decl (d: c_decl) : c_graph =
  match d.ddesc with
  | DVal (_, _, e) ->
     (match (type_of_schema d.dinfo).tdesc with
      | TArrow (_, _, (spawn_var, spawn_typ), (touch_var, touch_type), g) ->
         GPi (spawn_var, spawn_typ, touch_var, touch_type, g)
      | _ -> e.egr
     )
  | DExp e -> e.egr
  | DExtType _
  | DExtRecType _
  | DExternal _ -> GEmpty
  | DTypeDef (_, _, _) -> GEmpty

let really_infer_gr ?(init_state = InferGr.empty_grcheck_state) prog =
  match InferGr.grcheck_program ~init_state:init_state prog with
  | Ok ((c_prog : c_decl list), global_verts, x, y) ->
    message "graph bindings";
    let gr_bindings = (GrUtil.GrCtx.bindings x) in
    pprint (pprint_str_bindings pprint_g_graph) gr_bindings;
    message "type bindings";
    let ty_bindings = (InferTy.Ctx.bindings y) in
    pprint (pprint_str_bindings pprint_g_schema) ty_bindings;
    message "running global affinity check...";
    let global_gr = build_seq (List.map graph_of_decl c_prog) in
    let _ = affinity_check global_gr in
    message "program is affine.";
    c_prog, global_verts
  | Error e ->
    printnth e.geinspection_stack 4;
    printnth e.geinspection_stack 3;
    printnth e.geinspection_stack 2;
    printnth e.geinspection_stack 1;
    printnth e.geinspection_stack 0;
    pprint pprint_gr_error_desc e.gedesc;
    message "";
    raise (GraphError e)

let infer_gr ?(init_state = InferGr.empty_grcheck_state) prog =
  with_timer ginf_timer (really_infer_gr ~init_state:init_state) prog

let find_decl_by_id id prog =
  List.find
    (fun d ->
      match d.ddesc with
      | DVal (id', _, _) -> id = id'
      (* | DFun (_, id', _, _, _, _, _, _)  *)
      | DExp _
        | DExtRecType _
        | DExtType _
        | DExternal _ -> false
      | DTypeDef (_, _, _) -> false)
    prog

let var_of_decl d =
  match d.ddesc with
  | DVal (s, _, _) -> Id s
  | _ -> Id "-"

let run_analysis s f prog =
  if !func = ""
  then
    (* Run analysis on every binding *)
    List.iter
      (fun d ->
        log (Printf.sprintf "Starting %s on %s" s
               (string_of_longid (var_of_decl d)));
        f d;
        log (Printf.sprintf "Finished %s on %s" s
               (string_of_longid (var_of_decl d))))
      prog
  else
    (log (Printf.sprintf "Starting %s on %s" s !func);
     f (find_decl_by_id !func prog);
     log (Printf.sprintf "Finished %s on %s" s !func))


let print_timers out =
  let print_timer s timer =
    Printf.fprintf out "Time for %s:\t%.2fms\n" s !timer
  in
  print_timer "type inference" tinf_timer;
  print_timer "graph type inference" ginf_timer;
  (if !deadlock then print_timer "deadlock detection" dl_timer);
  (if !ws then print_timer "work-span estimate" ws_timer);
  (if !fj then print_timer "forj-join conversion" fj_timer)

let _ = Format.fprintf Format.std_formatter "\n"

let chan = open_in !fname
let parsed_program = parse chan
let _ = message "starting type inference"
let tycheck_result =
  try infer_ty parsed_program
  with InferTy.TypeError e -> Error e
let _ = message "type inference result:"
let ctx, tyname_ctx, t_prog = match tycheck_result with
  | Ok (ctx, tyname_ctx, con_ctx, t_prog) ->
    pprint (pprint_str_bindings pprint_t_schema) (InferTy.Ctx.bindings ctx);
    ctx, tyname_ctx, t_prog
  | Error e ->
    printnth e.teinspection_stack 4;
    printnth e.teinspection_stack 3;
    printnth e.teinspection_stack 2;
    printnth e.teinspection_stack 1;
    printnth e.teinspection_stack 0;
    pprint pprint_ty_error_desc e.tedesc;
    pprint_ty_error (!fname) e ;
    message "";
    raise (TypeError e)

let c_prog, global_verts =
  let init_state = InferGr.empty_grcheck_state in
  infer_gr ~init_state t_prog

let visualize exp out d =
  let g = graph_of_decl d in
  let g = Norm.expand_max exp g in
  Dot.write out (Dot.of_graph (g))


(* Do the requested operations *)

(* Output types *)
let _ =
  if not (!notypes) then
    List.iter
      (fun d ->
        if (match d.ddesc with | DVal _ | DExp _ -> true | _ -> false) then
          Format.fprintf Format.std_formatter
            "%s : %a | %a\n"
            (string_of_longid (var_of_decl d))
            pprint_c_schema d.dinfo
            pprint_c_graph
            (match d.ddesc with
             | DVal (_, _, e) | DExp e -> e.egr | _ -> GEmpty)
      )
      c_prog
(* Output sizes *)
let _ =
  if (!print_sizes) then
    List.iter
      (fun d ->
        if (match d.ddesc with | DVal _ | DExp _ -> true | _ -> false) then
          Format.fprintf Format.std_formatter
            "Size of graph for %s : %d\n"
            (string_of_longid (var_of_decl d))
            (Norm.real_size
               (match d.ddesc with
                | DVal (_, _, e) | DExp e -> e.egr | _ -> GEmpty)
            )
      )
      c_prog

(* Visualize *)
let _ =
  if !vis then
    let _ = log "Starting visualization" in
    let file =
      if !vis_out = "" then "out.dot" else !vis_out
    in
    let d =
      if !func = ""
      then
        List.nth c_prog ((List.length c_prog) - 1)
      else
        find_decl_by_id !func c_prog
    in
    (visualize 3 file d;
     log "Finished visualization")

let _ = if !timing then print_timers stdout

let _ = message ""
