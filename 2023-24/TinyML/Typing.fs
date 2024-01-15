(*
* TinyML
* Typing.fs: typing algorithms
*)

module TinyML.Typing

open Ast

let type_error fmt = throw_formatted TypeError fmt

type subst = (tyvar * ty) list

// TODO implement this
let unify (t1 : ty) (t2 : ty) : subst = []

// TODO implement this
let apply_subst (t : ty) (s : subst) : ty = t

// TODO implement this
let compose_subst (s1 : subst) (s2 : subst) : subst = s1 @ s2

// TODO implement this
let rec freevars_ty t = Set.empty

// TODO implement this
let freevars_scheme (Forall (tvs, t)) = Set.empty

// TODO implement this
let freevars_scheme_env env = Set.empty


// basic environment: add builtin operators at will
//

let gamma0 = [
    ("+", TyArrow (TyInt, TyArrow (TyInt, TyInt)))
    ("-", TyArrow (TyInt, TyArrow (TyInt, TyInt)))

]

// type inference
//

let rec typeinfer_expr (env : scheme env) (e : expr) : ty * subst =
    match e with
    | Lit (LInt _) -> TyInt, [] 
    | Lit (LBool _) -> TyBool, []
    | Lit (LFloat _) -> TyFloat, [] 
    | Lit (LString _) -> TyString, []
    | Lit (LChar _) -> TyChar, [] 
    | Lit LUnit -> TyUnit, []

    // TODO complete this implementation

    | _ -> unexpected_error "typeinfer_expr: unsupported expression: %s [AST: %A]" (pretty_expr e) e

// type checker
//
// optionally, a simple type checker (without type inference) could be implemented
// you might start implementing this for simplicity and eventually write the type inference once you gain familiarity with the code

let rec typecheck_expr (env : ty env) (e : expr) : ty =
    match e with
    | Lit (LInt _) -> TyInt
    | Lit (LFloat _) -> TyFloat
    | Lit (LString _) -> TyString
    | Lit (LChar _) -> TyChar
    | Lit (LBool _) -> TyBool
    | Lit LUnit -> TyUnit

    | Let (x, None, e1, e2) ->
        let t1 = typecheck_expr env e1
        let env' = (x, t1) :: env
        typecheck_expr env' e2

    | Let (x, Some t, e1, e2) ->
        let t1 = typecheck_expr env e1
        if t <> t1 then type_error "type %O differs from type %O in let-binding" t1 t 
        let env' = (x, t1) :: env
        typecheck_expr env' e2

    | Lambda (x, Some t, e) ->
        let env' = (x, t) :: env
        let te = typecheck_expr env' e
        TyArrow (t, te)

    | Lambda (x, None, e) ->
        type_error "unannotated lambdas are not supported by the type checker"


    // TODO optionally complete this implementation

    | _ -> unexpected_error "typecheck_expr: unsupported expression: %s [AST: %A]" (pretty_expr e) e
