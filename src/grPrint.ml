open Format
open Ast
open AstPrint
open GrUtil

let pprint_gr_subst f gr_subst =
  pprint (pprint_int_bindings pprint_g_graph) (Subst.bindings gr_subst)

let pprint_vs_subst f vs_subst =
  pprint (pprint_int_bindings pprint_g_vs) (Subst.bindings vs_subst)

let pprint_gr_error_desc f (e : gr_error_desc) = match e with
| GrErrUnboundVertexVar x ->
    fprintf f "GrErrUnboundVertexVar: %s" x
| GrErrUnboundGraphVar x ->
    fprintf f "GrErrUnboundGraphVar: %s" x
| GrErrUnboundTypeCon x ->
    fprintf f "GrErrUnboundTypeCon: %s" x
| GrErrUnimplemented x ->
    fprintf f "GrErrUnimplemented: %s" x
| GrErrResidualTyUnifVar ->
    fprintf f "GrErrResidualTyUnifVar: "
| GrErrUnknown x ->
    fprintf f "GrErrUnknown: %s" x
| GrErrCannotUnifyGrs (x, y) ->
    fprintf f "GrErrCannotUnifyGrs:@.  @[%a@]@.  @[%a@]"
      pprint_g_graph x
      pprint_g_graph y
| GrErrCannotUnifyVSs (x, y) ->
    fprintf f "GrErrCannotUnifyVSs: @.  @[%a@]@.  @[%a@]"
      pprint_g_vs x
      pprint_g_vs y
| GrErrCannotUnifyVSTs (x, y) ->
    fprintf f "GrErrCannotUnifyVSTs: @.  @[%a@]@.  @[%a@]"
      pprint_g_vs_typ x
      pprint_g_vs_typ y
| GrErrCannotUnifyTys (x, y) ->
    fprintf f "GrErrCannotUnifyTys: @.  @[%a@]@.  @[%a@]"
      pprint_g_typ x
      pprint_g_typ y
| GrErrMismatchedBinders -> fprintf f "GrErrMismatchedBinders"
                                    (* | x -> fprintf f "%a" pp_gr_error_desc x *)

let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let pprint_gr_error fname e =
  fprintf std_formatter "@.graph error in file ";
  match get_file_loc e.geinspection_stack with
  | None -> message "!!no file location!!"
  | Some l ->
    let start = (fst l).pos_cnum in
    let len = (snd l).pos_cnum - start in
    let filestring = read_whole_file fname in
    fprintf std_formatter "%s%a@." fname pprint_loc l;
    fprintf std_formatter "  `%s`@." (String.sub filestring start len);

