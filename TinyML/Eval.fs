(*
* TinyML
* Eval.fs: evaluator
*)

module TinyML.Eval

open Ast


let rec eval_expr (env : value env) (e : expr) : value =
    match e with
    | Lit lit -> VLit lit

    | _ -> unexpected_error "eval_expr: unsupported expression: %s [AST: %A]" (pretty_expr e) e