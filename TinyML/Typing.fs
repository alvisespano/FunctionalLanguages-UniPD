module TinyML.Typing

open Ast
open Printf


exception TypeError of string
exception UnexpectedError of string

let throw_formatted exnf fmt = ksprintf (fun s -> raise (exnf s)) fmt
let prefixed_error prefix (fmt : StringFormat<'a, 'r>) = throw_formatted TypeError (StringFormat<'a, 'r> (prefix + fmt.Value))

let unexpected_error fmt = prefixed_error "unexpected error" fmt
let type_error fmt = prefixed_error "type error" fmt

let rec typecheck_expr (env : ty env) (e : expr) : ty =
    match e with
    | Lit (LInt _) -> TyName "int"
    | Lit (LFloat n) -> TyName "float"
    | Lit (LString s) -> TyName "string"
    | Lit LUnit -> TyName "unit"

    | Lambda (x, None, e) -> unexpected_error "typecheck_expr: unannotated lambdas are not supported"
    
    | Lambda (x, Some t1, e) ->
        let t2 = typecheck_expr ((x, t1) :: env) e
        TyArrow (t1, t2)

    | App (e1, e2) ->
        let t1 = typecheck_expr env e1
        let t2 = typecheck_expr env e2
        match t1 with
        | TyArrow (l, r) ->
            if l = t2 then r 
            else type_error "wrong application: %s does not match %s" (pretty_ty t2) (pretty_ty l)
        | _ -> type_error "expecting an arrow on left side of application but got %s" (pretty_ty t1)

    | Let (x, e1, e2) ->
        let t1 = typecheck_expr env e1
        typecheck_expr ((x, t1) :: env) e2

    | IfThenElse (e1, e2, Some e3) ->
        let t1 = typecheck_expr env e1
        if t1 <> TyName "bool" then type_error "if condition must be a bool but got a %s" (pretty_ty t1)
        let t2 = typecheck_expr env e2
        let t3 = typecheck_expr env e3
        if t2 <> t3 then type_error"type mismatch in then (%s) and else (%s)" (pretty_ty t2) (pretty_ty t3)
        t2

    | Tuple es ->
        TyTuple (List.map (typecheck_expr env) es)
        
    | LetRec (f, tf, e1, e2) ->
        let env0 = (f, tf) :: env
        let t1 = typecheck_expr env1 e1
        match t1 with
        | TyArrow _ -> ()
        | _ -> failwithf "typecheck_expr: letrec is restricted to functions but got type %s" (pretty_ty t1)
        if t1 <> tf then failwithf "typecheck_expr: letrec type mismatch: expected %s but got %s" (pretty_ty tf) (pretty_ty t1)
        typecheck_expr env0 e2
