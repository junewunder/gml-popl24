open Ast

let simplify_recs = false
let simplify_seqs = true

let mk_seq g1 g2 =
  match (simplify_seqs, g1, g2) with
  | (true, GEmpty, _) -> g2
  | (true, _, GEmpty) -> g1
  | _ -> (GSeq (g1, g2))

let mk_or g1 g2 =
  match (g1, g2) with
  | (GEmpty, GEmpty) -> GEmpty
  | _ -> (GOr (g1, g2))

let mk_par g1 g2 = (GPar (g1, g2))

let mk_fut g u = (GFut (g, u))

let mk_touch u = (GTouch u)

let mk_pi uf uft ut utt g =
  (*match g with
  | GEmpty -> GEmpty
  | _ -> *)
  (GPi (uf, uft, ut, utt, g))

let mk_app g uf ut =
  (*match (uf, ut) with
  | ([], []) -> g
  | _ -> *)(GApp (g, uf, ut))

let rec has_par gv g =
  match g with
  | GEmpty -> false
  | GVar v' -> v' <> gv
  | GSeq (g1, g2)
    | GOr (g1, g2) -> (has_par gv g1) || (has_par gv g2)
  | GPi (_, _, _, _, g)
    | GNew (_, _, g)
    | GApp (g, _, _) -> has_par gv g
  | GRec (gv', g) -> has_par gv' g
  | _ -> true

let mk_rec gv g =
  if (not simplify_recs) || has_par gv g then (GRec (gv, g))
  else GEmpty

let mk_new u ut g =
  (GNew (u, ut, g))

let rec copy g =
  (match g with
     | GEmpty | GVar _ | GUVar _ | GTouch _ -> g
     | GSeq (g1, g2) -> GSeq (copy g1, copy g2)
     | GOr (g1, g2) -> GOr (copy g1, copy g2)
     | GPar (g1, g2) -> GPar (copy g1, copy g2)
     | GFut (g, u) -> GFut (copy g, u)
     | GPi (uf, uft, ut, utt, g) -> GPi (uf, uft, ut, utt, copy g)
     | GRec (gv, g) -> GRec (gv, copy g)
     | GNew (u, ut, g) -> GNew (u, ut, copy g)
     | GApp (g, uf, ut) -> GApp (copy g, uf, ut)
    )

let is_empty g =
  match g with
  | GEmpty -> true
  | _ -> false

let rec simplify ?(apps=false) ?(seqs=true) ?(recs=false)  g =
  match g with
  | GEmpty
    | GVar _
    | GUVar _ -> g
  | GSeq (g1, g2) ->
     let (sg1, sg2) = (simplify g1, simplify g2) in
     if seqs then
       (match (is_empty sg1, is_empty sg2) with
        | (true, _) -> sg2
        | (_, true) -> sg1
        | _ -> mk_seq sg1 sg2
       )
     else
       mk_seq sg1 sg2
  | GOr (g1, g2) ->
     let (sg1, sg2) = (simplify g1, simplify g2) in
     (match (is_empty sg1, is_empty sg2) with
      | (true, true) -> GEmpty
      | _ -> mk_or sg1 sg2
     )
  | GPar (g1, g2) -> mk_par (simplify g1) (simplify g2)
  | GFut (g, v) -> mk_fut (simplify g) v
  | GTouch v -> mk_touch v
  (* | GPi (_, VSTProd [], _, VSTProd [], g) when apps -> simplify g *)
  | GPi (uf, uft, ut, utt, g) ->
     let g = simplify g in
     if is_empty g && apps then GEmpty
     else (GPi (uf, uft, ut, utt, g))
  | GRec (v, g) ->
     let g = simplify g in
     if has_par v g || not recs then (GRec (v, g))
     else GEmpty
  (* | GNew (_, VSTProd [], g) -> simplify g *)
  | GNew (vs, ut, g) ->
     let g = simplify g in
     if is_empty g then GEmpty
     else (GNew (vs, ut, g))
  (* XXX
| GApp (g, mk_vs (VSTuple []), mk_vs (VSTuple [])) when apps -> simplify g
   *)
  | GApp (g, uf, ut) ->
     let g = simplify g in
     if is_empty g && apps then GEmpty
     else (GApp (g, uf, ut))

