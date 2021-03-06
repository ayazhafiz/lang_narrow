open Language
open Ty
open Typecheck

exception No_rule_applies

(** [subst bod x y] applies the substitution [x->y]bod
    Note: presently we do not check for name conflicts.
    We should use de Bruijn indeces or something here, but I want to keep this
    simple.
 *)
let rec subst bod x y =
  match bod with
  | Var n when n = x -> y
  | (Var _ as e) | (String _ as e) | (Nat _ as e) | (Bool _ as e) -> e
  | App (fn, args) -> App (subst fn x y, List.map (fun a -> subst a x y) args)
  | Narrow (e, ty) -> Narrow (subst e x y, ty)
  | If (cond, left, right) ->
      If (subst cond x y, subst left x y, subst right x y)
  | Record { fields; ty } ->
      Record { fields = OrdSMap.map (fun v -> subst v x y) fields; ty }
  | RecordProj (rcd, key) -> RecordProj (subst rcd x y, key)
  | RecordNarrow (key, rcd) -> RecordNarrow (key, subst rcd x y)

let is_val t =
  match t with String _ | Nat _ | Bool _ | Record _ -> true | _ -> false

let rec small_step ctx t =
  match t with
  | Var _ | String _ | Nat _ | Bool _ | Record _ -> raise No_rule_applies
  | App (Var fn, args) when List.for_all is_val args -> (
      match Ctx.find_opt fn ctx with
      | Some (BindFn (Fn (_, params, _, body), _)) ->
          let body1 = List.fold_left2 subst body (List.map fst params) args in
          body1
      | _ -> raise No_rule_applies )
  | App (fn, args) ->
      let rec lower_single seq =
        match seq with
        | [] -> []
        | a :: rest when not (is_val a) -> small_step ctx a :: rest
        | a :: rest -> a :: lower_single rest
      in
      App (fn, lower_single args)
  | Narrow (e, ty) when is_val e ->
      let tyE = typecheck ctx e in
      let res =
        match ty with TyUnion u -> TySet.exists (tyeq tyE) u | t -> tyeq tyE t
      in
      Bool res
  | Narrow (e, ty) -> Narrow (small_step ctx e, ty)
  | If (Bool true, left, _) -> left
  | If (Bool false, _, right) -> right
  | If (cond, left, right) -> If (small_step ctx cond, left, right)
  | RecordProj (Record { fields; _ }, key) -> OrdSMap.find key fields
  | RecordProj (rcd, key) -> RecordProj (small_step ctx rcd, key)
  | RecordNarrow (field, Record { fields; _ }) ->
      Bool (OrdSMap.mem field fields)
  | RecordNarrow (field, rcd) -> RecordNarrow (field, small_step ctx rcd)

let rec eval ctx t =
  try
    let t' = small_step ctx t in
    eval ctx t'
  with No_rule_applies -> t
