open Language
open Lexing
open Typecheck

let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse lexbuf =
  let result =
    try Some (Parser.toplevel Lexer.read lexbuf) with
    | Lexer.SyntaxError msg ->
        Printf.eprintf "Syntax error: %s at %s\n" msg
          (string_of_position lexbuf);
        None
    | _ ->
        Printf.eprintf "Syntax error at %s\n" (string_of_position lexbuf);
        None
  in
  Parsing.clear_parser ();
  result

let ends_with s1 s2 =
  let n1 = String.length s1 and n2 = String.length s2 in
  n1 >= n2 && String.sub s1 (n1 - n2) n2 = s2

let readin buf =
  try
    while true do
      let line = read_line () in
      if ends_with line ";;" then (
        let trim = String.sub line 0 (String.length line - 2) in
        buf := !buf @ [ trim ];
        raise End_of_file )
      else buf := !buf @ [ line ]
    done
  with End_of_file -> Printf.printf "\n"

let pr_binding what ty = Printf.sprintf "%s :: %s" what ty

let process_fn (env, bindings) (Fn (name, _, _, _) as fn) =
  let ty = typecheck_fn env fn in
  let bind = pr_binding name (string_of_ty ty) in
  (Env.add name ty env, bindings @ [ bind ])

let process_expr_opt (env, bindings) expr =
  match expr with
  | None -> (env, bindings)
  | Some expr ->
      let ty = typecheck env expr in
      let bind = pr_binding (string_of_expr expr) (string_of_ty ty) in
      (env, bindings @ [ bind ])

let rec repl () =
  Printf.printf "> ";
  flush_all ();
  let buf = ref [] in
  readin buf;
  let input = String.concat "\n" !buf in
  let parsed = parse (Lexing.from_string ~with_positions:true input) in
  ( match parsed with
  | None -> ()
  | Some { fns; expr } -> (
      try
        let env, bindings = List.fold_left process_fn (Env.empty, []) fns in
        let _, bindings1 = process_expr_opt (env, bindings) expr in
        Printf.printf "%s" (String.concat "\n" bindings1)
      with Failure msg -> Printf.eprintf "%s" msg ) );
  flush_all ();
  Printf.printf "\n\n";
  flush_all ();
  repl ()

let () = repl ()
