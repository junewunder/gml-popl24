open Format
open Ast
open AstPrint
open InferTy

let pprint_ty_error_desc f (e : ty_error_desc) = match e with
| TyErrUnboundVar x ->
  fprintf f "TyErrUnboundVar: %s" x
| TyErrCannotUnify (t1, t2) ->
  fprintf f "TyErrCannotUnify@.  @[%a@]@.  @[%a@]"
    pprint_t_typ t1
    pprint_t_typ t2
| TyErrUnimplemented x ->
  fprintf f "TyErrUnimplemented: %s" x
| TyErrUnknown x ->
  fprintf f "TyErrUnknown: %s" x
| TyErrInvalidPattern ->
  fprintf f "TyErrInvalidPattern"
| TyErrCantGeneralize ->
  fprintf f "TyErrCantGeneralize"
| TyErrCantInstantiateWithFuture ->
  fprintf f "TyErrCantInstantiateWithFuture"

let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let pprint_ty_error fname e =
  fprintf std_formatter "@.type error in file ";
  match get_file_loc e.teinspection_stack with
  | None -> message "!!no file location!!"
  | Some l ->
    let start = (fst l).pos_cnum in
    let len = (snd l).pos_cnum - start in
    let filestring = read_whole_file fname in
    fprintf std_formatter "%s%a@." fname pprint_loc l;
    fprintf std_formatter "  `%s`@." (String.sub filestring start len);

