(*
* TinyML
* Eval.fs: evaluator
*)

module TinyML.Eval

open Ast

exception NotFound

let lookup env x = 
    let _, v = List.find (fun (x', v) -> x = x') env
    v

// evaluator
//

let rec eval_expr (venv : value env) (e : expr) : value =
    match e with
    | Lit lit -> VLit lit

    | Lambda (x, _, e) -> Closure (venv, x, e)

    | App (e1, e2) -> 
        let v1 = eval_expr venv e1
        let v2 = eval_expr venv e2
        match v1 with
        | Closure (venv', x, e) ->
            let venv' = (x, v2) :: venv'
            eval_expr venv' e

        | _ -> unexpected_error "non-closure on left hand of application"

    | Var x -> lookup venv x
    

    // TODO complete this implementation

    | _ -> unexpected_error "eval_expr: unsupported expression: %s [AST: %A]" (pretty_expr e) e
