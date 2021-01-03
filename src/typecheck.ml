open Language

let rec tyeq t1 t2 =
  match (t1, t2) with
  | TyNever, TyNever | TyNat, TyNat | TyString, TyString -> true
  | TyFn (p1, r1), TyFn (p2, r2) -> List.for_all2 tyeq p1 p2 && tyeq r1 r2
  | _, _ -> false

let typecheck env expr =
  match expr with
  | Var n when Env.mem n env -> Env.find n env
  | Var n -> failwith ("Unbound variable \"" ^ n ^ "\"")
  | String _ -> TyString
  | Nat _ -> TyNat

let typecheck_fn env (Fn (name, params, retTy, body)) =
  let env1 = List.fold_left (fun e (p, t) -> Env.add p t e) env params in
  let bodyTy = typecheck env1 body in
  if tyeq bodyTy retTy then TyFn (List.map snd params, retTy)
  else
    failwith
      (Printf.sprintf
         "\"%s\" is mistyped: typed as \"%s\", but body verified as \"%s\"" name
         (string_of_ty retTy) (string_of_ty bodyTy))
