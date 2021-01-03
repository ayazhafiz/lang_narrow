open Language

type cExpr =
  | CIdent of string
  | CNat of int
  | CString of string
  | CBool of bool
  | CCall of cExpr * cExpr list

type cDecl = CDecl of cExpr * cExpr option

type cStmt =
  | CDeclStmt of cDecl
  | CAssign of cExpr * cExpr
  | CExprStmt of cExpr
  | CReturn of cExpr
  | CIf of cExpr * cBlock * cBlock

and cBlock = cStmt list

type cType = CTyTaggedAny | CTyInt  (** Return type of main *)

(** cFn is a C function with a name, parameter list, and a return expression.
    Any parameters are always of type "tagged_any".
 *)
type cFn = CFn of cType * cExpr * cDecl list * cBlock

let rt_make_nat e = CCall (CIdent "make_nat", [ e ])

let rt_make_string e = CCall (CIdent "make_string", [ e ])

let rt_make_bool e = CCall (CIdent "make_bool", [ e ])

let rt_print e = CCall (CIdent "print", [ e ])

let rt_is_tag e ty =
  let tyTagMacro =
    match ty with
    | TyNat -> "NAT"
    | TyString -> "STRING"
    | TyBool -> "BOOL"
    | t ->
        failwith
          (Printf.sprintf "No runtime type tag for \"%s\"" (string_of_ty t))
  in
  CCall (CIdent "is", [ e; CIdent tyTagMacro ])

(*      *)
(* Emit *)
(*      *)

let emit_cTy ty = match ty with CTyInt -> "int" | CTyTaggedAny -> "tagged_any"

let rec emit_cExpr e =
  match e with
  | CIdent s -> s
  | CNat n -> string_of_int n
  | CString s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | CBool true -> "1"
  | CBool false -> "0"
  | CCall (n, args) ->
      Printf.sprintf "%s(%s)" (emit_cExpr n)
        (String.concat ", " (List.map emit_cExpr args))

let emit_cDecl (CDecl (n, e)) =
  let ty = CTyTaggedAny in
  let init =
    match e with None -> "" | Some e -> Printf.sprintf " = %s" (emit_cExpr e)
  in
  Printf.sprintf "%s %s%s" (emit_cTy ty) (emit_cExpr n) init

let rec emit_cStmt indent s =
  let indentS = String.init indent (fun _ -> ' ') in
  let emitS =
    match s with
    | CDeclStmt d -> Printf.sprintf "%s;" (emit_cDecl d)
    | CExprStmt e -> Printf.sprintf "%s;" (emit_cExpr e)
    | CReturn e -> Printf.sprintf "return %s;" (emit_cExpr e)
    | CAssign (e1, e2) ->
        Printf.sprintf "%s = %s;" (emit_cExpr e1) (emit_cExpr e2)
    | CIf (cond, left, right) ->
        let bLeft = List.map (emit_cStmt (indent + 2)) left in
        let bRight = List.map (emit_cStmt (indent + 2)) right in
        let parts =
          [ Printf.sprintf "if (%s) {" (emit_cExpr cond) ]
          @ bLeft @ [ "} else {" ] @ bRight @ [ "}" ]
        in
        String.concat "\n" (List.map (fun s -> indentS ^ s) parts)
  in
  indentS ^ emitS

let emit_cFn indent (CFn (ty, name, params, block)) =
  let indentS = String.init indent (fun _ -> ' ') in
  let block = List.map (emit_cStmt (indent + 2)) block in
  (* Initialize all params as type of tagged_any, like everything else. *)
  let params = List.map emit_cDecl params in
  let header =
    Printf.sprintf "%s %s(%s) {" (emit_cTy ty) (emit_cExpr name)
      (String.concat ", " params)
  in
  let footer = "}" in
  String.concat "\n"
    (List.map (fun s -> indentS ^ s) ((header :: block) @ [ footer ]))

(*         *)
(* Codegen *)
(*         *)

(* TODO: real unique and non-colliding identifiers *)
let genCIdent originalId = CIdent ("_" ^ originalId)

let genUniqCIdent =
  let counter = ref 0 in
  fun () ->
    let fresh = CIdent ("_fresh_" ^ string_of_int !counter) in
    counter := !counter + 1;
    fresh

(* codegen_expr :: expr -> (cStmt list, cExpr) *)
let rec codegen_expr expr =
  match expr with
  | Var n -> ([], genCIdent n)
  | Nat n -> ([], rt_make_nat (CNat n))
  | String s -> ([], rt_make_string (CString s))
  | Bool b -> ([], rt_make_bool (CBool b))
  | App (n, args) ->
      let stmts, call = codegen_expr n in
      let stmts, args =
        List.fold_right
          (fun arg (stmts, cArgs) ->
            let stmts1, cA = codegen_expr arg in
            (stmts1 @ stmts, cA :: cArgs))
          args (stmts, [])
      in
      (stmts, CCall (call, args))
  | Narrow (e, ty) ->
      let stmts, e = codegen_expr e in
      (stmts, rt_is_tag e ty)
  | If (cond, left, right) ->
      let outV = genUniqCIdent () in
      let stmtsCond, cCond = codegen_expr cond in

      let stmtsL, cLeft = codegen_expr left in
      let outLeft = CAssign (outV, cLeft) in
      let blockLeft = stmtsL @ [ outLeft ] in

      let stmtsR, cRight = codegen_expr right in
      let outRight = CAssign (outV, cRight) in
      let blockRight = stmtsR @ [ outRight ] in

      let cIfSeq =
        [ CDeclStmt (CDecl (outV, None)); CIf (cCond, blockLeft, blockRight) ]
      in
      (stmtsCond @ cIfSeq, outV)

let codegen_fn (Fn (name, params, _, body)) =
  let stmts, bodyExpr = codegen_expr body in
  let body = stmts @ [ CReturn bodyExpr ] in
  CFn
    ( CTyTaggedAny,
      genCIdent name,
      List.map (fun (p, _) -> CDecl (genCIdent p, None)) params,
      body )

(** Generates C code for the program, excluding the runtime code.
    Useful for checking C codegen in the repl.
 *)
let codegen_c fns expr =
  let cFns = List.map codegen_fn fns in
  let stmts1, cExpr = codegen_expr expr in
  let stmts2, exprVar = codegen_expr (Var "_main_result") in
  let mainN = CIdent "main" in
  let cMain =
    CFn
      ( CTyInt,
        mainN,
        [],
        stmts1 @ stmts2
        @ [
            CDeclStmt (CDecl (exprVar, Some cExpr));
            CExprStmt (rt_print exprVar);
          ] )
  in
  let topLevels = List.map (emit_cFn 0) (cFns @ [ cMain ]) in
  String.concat "\n" topLevels

(** Generates C code with the runtime prepended. *)
let codegen_c_w_rt fns expr =
  let userCode = codegen_c fns expr in
  let runtime = "src/runtime.c" in
  let runtime =
    try open_in "src/runtime.c"
    with _ ->
      failwith (Printf.sprintf "Failed to find runtime at \"%s\"" runtime)
  in
  let runtime = really_input_string runtime (in_channel_length runtime) in
  String.concat "\n" [ runtime; "// User code"; userCode ]
