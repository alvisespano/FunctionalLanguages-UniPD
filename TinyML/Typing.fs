﻿(*
* TinyML
* Typing.fs: typing algorithms
*)

module TinyML.Typing

open Ast

let type_error fmt = throw_formatted TypeError fmt

// define a new variable for floats
let TyFloat = TyName "float"
// define an active pattern for floats
let (|TyFloat|_|) (t : ty) =
    match t with
    | TyName "float" -> Some ()
    | _ -> None


let rec typecheck_expr (env : ty env) (e : expr) : ty =
    match e with
    | Lit (LInt _) -> TyName "int"
    | Lit (LFloat _) -> TyName "float"
    | Lit (LString _) -> TyName "string"
    | Lit (LChar _) -> TyName "char"
    | Lit (LBool _) -> TyName "bool"
    | Lit LUnit -> TyName "unit"

    | Var x ->
        let _, t = List.find (fun (y, _) -> x = y) env
        t

    | Lambda (x, None, e) -> unexpected_error "typecheck_expr: unannotated lambda is not supported"
    
    | Lambda (x, Some t1, e) ->
        let t2 = typecheck_expr ((x, t1) :: env) e
        TyArrow (t1, t2)

    | App (e1, e2) ->
        let t1 = typecheck_expr env e1
        let t2 = typecheck_expr env e2
        match t1 with
        | TyArrow (l, r) ->
            if l = t2 then r 
            else type_error "wrong application: argument type %s does not match function domain %s" (pretty_ty t2) (pretty_ty l)
        | _ -> type_error "expecting a function on left side of application but got %s" (pretty_ty t1)

    | Let (x, e1, e2) ->
        let t1 = typecheck_expr env e1
        typecheck_expr ((x, t1) :: env) e2

    | IfThenElse (e1, e2, Some e3) ->
        let t1 = typecheck_expr env e1
        if t1 <> TyName "bool" then type_error "if condition must be a bool but got a %s" (pretty_ty t1)
        let t2 = typecheck_expr env e2
        let t3 = typecheck_expr env e3
        if t2 <> t3 then type_error "type mismatch in if-then-else: then branch has type %s and is different from else branch type %s" (pretty_ty t2) (pretty_ty t3)
        t2

    | Tuple es ->
        TyTuple (List.map (typecheck_expr env) es)

    | LetRec (f, None, e1, e2) ->
        unexpected_error "typecheck_expr: unannotated let rec is not supported"
        
    | LetRec (f, Some tf, e1, e2) ->
        let env0 = (f, tf) :: env
        let t1 = typecheck_expr env0 e1
        match t1 with
        | TyArrow _ -> ()
        | _ -> type_error "let rec is restricted to functions but got type %s" (pretty_ty t1)
        if t1 <> tf then type_error "let rec type mismatch: expected %s but got %s" (pretty_ty tf) (pretty_ty t1)
        typecheck_expr env0 e2

    | BinOp (e1, ("+" | "-" | "/" | "%" | "*" as op), e2) ->
        let t1 = typecheck_expr env e1
        let t2 = typecheck_expr env e2
        match t1, t2 with
        | TyName "int", TyName "int" -> TyName "int"
        | TyName "float", TyName "float" -> TyName "float"
        | TyName "int", TyName "float"
        | TyName "float", TyName "int" -> TyName "float"
        | _ -> type_error "binary operator expects two int operands but got %s %s %s" (pretty_ty t1) op (pretty_ty t2)
        

    | BinOp (e1, ("<" | "<=" | ">" | ">=" | "=" | "<>" as op), e2) ->
        let t1 = typecheck_expr env e1
        let t2 = typecheck_expr env e2
        match t1, t2 with
        | TyName "int", TyName "int" -> ()
        | _ -> type_error "binary operator expects two numeric operands but got %s %s %s" (pretty_ty t1) op (pretty_ty t2)
        TyName "bool"

    | BinOp (e1, ("and" | "or" as op), e2) ->
        let t1 = typecheck_expr env e1
        let t2 = typecheck_expr env e2
        match t1, t2 with
        | TyName "bool", TyName "bool" -> ()
        | _ -> type_error "binary operator expects two bools operands but got %s %s %s" (pretty_ty t1) op (pretty_ty t2)
        TyName "bool"

    | UnOp ("not", e) ->
        let t = typecheck_expr env e
        if t <> TyName "bool" then 
            type_error "unary not expects a bool operand but got %s" (pretty_ty t)
            
    | UnOp ("-", e) ->
        let t = typecheck_expr env e
        match t with
        | TyName "int" -> TyName "int"
        | TyName "float" -> TyName "float"
        | _ -> type_error "unary negation expects a numeric operand but got %s" (pretty_ty t)


    //| _ -> unexpected_error "typecheck_expr: unsupported expression: %s [AST: %A]" (pretty_expr e) e