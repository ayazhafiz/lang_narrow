open Language

let fail_not_function_ty e =
  failwith
    (Printf.sprintf
       "Application of \"%s\" is unsound as it is not a function type"
       (string_of_expr e))

let fail_param_mismatch e args argTys =
  failwith
    (Printf.sprintf
       "Application of \"%s\" with \"(%s)\" is unsound as the call differs \
        from the function signature"
       (string_of_expr e)
       (String.concat ", "
          (List.map2
             (fun p t ->
               Printf.sprintf "%s: %s" (string_of_expr p) (string_of_ty t))
             args argTys)))

let fail_mistyped name expected real =
  failwith
    (Printf.sprintf
       "\"%s\" is mistyped: typed as \"%s\", but verified as \"%s\"" name
       (string_of_ty expected) (string_of_ty real))

let fail_if_narrow_of_non_var e =
  failwith
    (Printf.sprintf
       "Use of non-variable expression \"%s\" in conditional expression is not \
        permitted"
       (string_of_expr e))

let fail_if_wrong_type t =
  failwith
    (Printf.sprintf
       "Type of \"if\" condition must be a bool or narrowing type; found \"%s\""
       (string_of_ty t))

(*                                            *)
(* Error messages above, below real typecheck *)
(*                                            *)

let rec tyeq t1 t2 =
  match (t1, t2) with
  | TyUnknown, TyUnknown
  | TyNever, TyNever
  | TyNat, TyNat
  | TyString, TyString
  | TyBool, TyBool ->
      true
  | TyFn (p1, r1), TyFn (p2, r2) -> List.for_all2 tyeq p1 p2 && tyeq r1 r2
  | TyNarrowed _, _ | _, TyNarrowed _ ->
      failwith "Narrowed types cannot be compared"
  | _, _ -> false

(** Returns [true] iff [tyS <: tyT] *)
let rec is_subtype ctx tyS tyT =
  tyeq tyS tyT
  ||
  match (tyS, tyT) with
  | _, TyUnknown -> true
  | TyNever, _ -> true
  | TyUnion fieldsS, tyT ->
      List.for_all (fun tyS -> is_subtype ctx tyS tyT) fieldsS
  | ty, TyUnion fields -> List.exists (is_subtype ctx ty) fields
  | TyFn (pS, rS), TyFn (pT, rT) ->
      (* Admission of functions is contravariant on parameters and covariant on return types:
           (nat|string): nat <: (nat): nat|string
      *)
      List.length pS = List.length pT
      && List.for_all2 (fun pS pT -> is_subtype ctx pT pS) pS pT
      && is_subtype ctx rS rT
  | _, _ -> false

let simplifyUnion tys =
  match tys with [] -> TyNever | [ ty ] -> ty | tys -> TyUnion tys

(** [join ctx ty1 ty2] finds the least upper bound (common supertype) of two types. *)
let rec join ctx ty1 ty2 =
  if is_subtype ctx ty1 ty2 then ty2
  else if is_subtype ctx ty2 ty1 then ty1
  else
    match (ty1, ty2) with
    | TyFn (p1, r1), TyFn (p2, r2) when List.length p1 = List.length p2 ->
        (* [(nat|string|bool)->(string|bool)] J [(string|bool)->string] yields [(string|bool)->(string|bool)] *)
        let joinParams = List.map2 (meet ctx) p1 p2 in
        let joinRet = join ctx r1 r2 in
        TyFn (joinParams, joinRet)
    | TyUnion f1, TyUnion f2 ->
        (* nat|string ^ string|bool yields nat|string|bool *)
        let allFields =
          List.append f1
            (List.filter (fun l -> not (List.exists (tyeq l) f1)) f2)
        in
        simplifyUnion allFields
    | (TyUnion _ as uTy), singleTy | singleTy, (TyUnion _ as uTy) ->
        join ctx uTy (TyUnion [ singleTy ])
    | singleTy1, singleTy2 -> TyUnion [ singleTy1; singleTy2 ]

(** [meet ctx ty1 ty2] finds the greatest lower bound (common subtype) of two types. *)
and meet ctx ty1 ty2 =
  if is_subtype ctx ty1 ty2 then ty1
  else if is_subtype ctx ty2 ty1 then ty2
  else
    match (ty1, ty2) with
    | TyFn (p1, r1), TyFn (p2, r2) when List.length p1 = List.length p2 ->
        (* [(nat|string|bool)->(string|bool)] ^ [(string|bool)->string] yields [(nat|string|bool)->string] *)
        let meetParams = List.map2 (join ctx) p1 p2 in
        let meetRet = meet ctx r1 r2 in
        TyFn (meetParams, meetRet)
    | TyUnion f1, TyUnion f2 ->
        (* nat|string ^ string|bool yields string *)
        let common = List.filter (fun l -> List.exists (tyeq l) f2) f1 in
        simplifyUnion common
    | (TyUnion _ as uTy), singleTy | singleTy, (TyUnion _ as uTy) ->
        meet ctx uTy (TyUnion [ singleTy ])
    | _, _ -> TyNever

(** [exclude ctx tyB tyE] excludes from [tyB] the type [tyE].
  TODO: should we distribute over unions rather than operating on them?
  *)
let rec exclude ctx tyB tyE =
  if tyeq tyB tyE then TyNever
  else
    match (tyB, tyE) with
    | TyFn (pB, rB), TyFn (pE, rE) when List.length pB = List.length pE ->
        (* [(nat|string) -> (string|bool)] - [(nat) -> bool] yields [(string) -> bool] (i think?) *)
        let excParams = List.map2 (exclude ctx) pB pE in
        let excRet = exclude ctx rB rE in
        TyFn (excParams, excRet)
    | TyUnion fB, TyUnion fE ->
        (* (nat|string) - string yields nat *)
        let filtered =
          List.filter (fun l -> not (List.exists (tyeq l) fE)) fB
        in
        simplifyUnion filtered
    | (TyUnion _ as uTy), singleTy -> exclude ctx uTy (TyUnion [ singleTy ])
    | singleTy, (TyUnion _ as uTy) -> exclude ctx (TyUnion [ singleTy ]) uTy
    | tyB, _ -> tyB

(** [typecheck ctx expr] returns the type of [expr], raising [Failure] if there
 are any type errors. *)
let rec typecheck ctx expr =
  match expr with
  | Var n when Ctx.mem n ctx -> Ctx.typeof n ctx
  | Var n -> failwith ("Unbound variable \"" ^ n ^ "\"")
  | String _ -> TyString
  | Nat _ -> TyNat
  | Bool _ -> TyBool
  | App (n, args) -> (
      match typecheck ctx n with
      | TyFn (params, ret) ->
          let argTys = List.map (typecheck ctx) args in
          if
            List.length params = List.length argTys
            && List.for_all2 (is_subtype ctx) argTys params
          then ret
          else fail_param_mismatch n args argTys
      | _ -> fail_not_function_ty n )
  | Narrow (expr, ty) ->
      (* Consider the type narrowings

           a: string|nat
           a is string #-> TyNarrow (a, string, nat)

           b: string
           b is nat    #-> TyNarrow (b, never, nat)

         When the type is narrowed to the narrowing type, the resulting left
         (narrowed) type is the meet of the expression type and the narrowing
         type

           a_L: string|nat ^ string = string
           b_L: string     ^ nat    = never

         The right (unnarrowed) type is expression type excluding the narrowing type

           a_R: string|nat - string = nat
           b_R: string     - nat    = string
      *)
      let tyExpr = typecheck ctx expr in
      let tyLeft = meet ctx tyExpr ty in
      let tyRight = exclude ctx tyExpr ty in
      TyNarrowed (expr, tyLeft, tyRight)
  | If (cond, left, right) -> (
      match typecheck ctx cond with
      | TyNarrowed (Var n, tyLeft, tyRight) ->
          let ctxLeft = Ctx.add_var n tyLeft ctx in
          let tyLeft = typecheck ctxLeft left in
          let ctxRight = Ctx.add_var n tyRight ctx in
          let tyRight = typecheck ctxRight right in
          join ctx tyLeft tyRight
      | TyNarrowed (e, _, _) -> fail_if_narrow_of_non_var e
      | TyBool ->
          let tyLeft = typecheck ctx left in
          let tyRight = typecheck ctx right in
          join ctx tyLeft tyRight
      | t -> fail_if_wrong_type t )

(** [typecheck_fn ctx fn] returns the type of [fn], raising [Failure] if there
 are any type errors. *)
let typecheck_fn ctx (Fn (name, params, retTy, body)) =
  let ctx = List.fold_left (fun e (p, t) -> Ctx.add_var p t e) ctx params in
  let bodyTy = typecheck ctx body in
  let paramTys = List.map snd params in
  if is_subtype ctx bodyTy retTy then TyFn (paramTys, retTy)
  else fail_mistyped name (TyFn (paramTys, retTy)) (TyFn (paramTys, bodyTy))
