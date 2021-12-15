module Lesson17_18_19_20

type tyvar = int

type ty =
    | TyName of string
    | TyArrow of ty * ty
    | TyVar of tyvar
    | TyTuple of ty list

type scheme = Forall of tyvar list * ty

type lit = LInt of int
         | LFloat of float
         | LString of string
         | LUnit 

type expr = 
    | Lit of lit
    | Lambda of string * ty option * expr
    | App of expr * expr
    | Var of string
    | Let of string * expr * expr
    | LetRec of string * ty * expr * expr
    | IfThenElse of expr * expr * expr option
    | Tuple of expr list
    
type 'a env = (string * 'a) list  

type value =
    | VLit of lit
    | VTuple of value list
    | Closure of value env * string * expr
    | RecClosure of value env * string * string * expr



// (1, 2, 3)

//let test_expr = Tuple [Lit (LInt 1); Lit (LInt 2); Lit (LInt 3)]
//let test_expr = Tuple [for i = 1 to 3 do yield Lit (LInt i)]

let test_expr = Tuple (List.map (fun n -> Lit (LInt n)) [1..3])

let rec pretty_ty t =
    match t with
    | TyName s -> s
    | TyArrow (t1, t2) -> sprintf "%s -> %s" (pretty_ty t1) (pretty_ty t2)
    | TyVar n -> sprintf "'%d" n

let rec pretty_expr e =
    match e with
    | Lit (LInt n) -> sprintf "%d" n
    | Lit (LFloat n) -> sprintf "%g" n
    | Lit (LString s) -> s
    | Lit LUnit -> "()"
    
    | Lambda (x, None, e) -> sprintf "fun %s -> %s" x (pretty_expr e)
    | Lambda (x, Some t, e) -> sprintf "fun (%s : %s) -> %s" x (pretty_ty t) (pretty_expr e)
    
    // TODO pattern-match sub-application cases
    | App (e1, e2) -> sprintf "%s %s" (pretty_expr e1) (pretty_expr e2)

    | Var x -> x

    | Let (x, e1, e2) ->
        sprintf "let %s = %s in %s" x (pretty_expr e1) (pretty_expr e2)

    | LetRec (f, tf, e1, e2) ->
        sprintf "let rec %s : %s = %s in %s"
            f (pretty_ty tf)
            (pretty_expr e1) (pretty_expr e2)

    | IfThenElse (e1, e2, e3o) ->
        let s = sprintf "if %s then %s" (pretty_expr e1) (pretty_expr e2)
        match e3o with
        | None -> s
        | Some e3 -> sprintf "%s else %s" s (pretty_expr e3)
        
    | Tuple es ->
        let rec f es =
            match es with
            | [] -> failwith "pretty_expr: empty expr list in tuple"
            | [e] -> pretty_expr e
            | e :: es -> sprintf "%s, %s" (pretty_expr e) (f es)
        sprintf "(%s)" (f es)

let type_error msg = failwith (sprintf "type error: %s" msg)

let rec typecheck_expr (env : ty env) (e : expr) : ty =
    match e with
    | Lit (LInt _) -> TyName "int"
    | Lit (LFloat n) -> TyName "float"
    | Lit (LString s) -> TyName "string"
    | Lit LUnit -> TyName "unit"

    | Lambda (x, None, e) -> failwith "typecheck_expr: unannotated lambdas are not supported"
    
    | Lambda (x, Some t1, e) ->
        let t2 = typecheck_expr ((x, t1) :: env) e
        TyArrow (t1, t2)

    | App (e1, e2) ->
        let t1 = typecheck_expr env e1
        let t2 = typecheck_expr env e2
        match t1 with
        | TyArrow (l, r) ->
            if l = t2 then r 
            else type_error (sprintf "wrong application: %s does not match %s" (pretty_ty t2) (pretty_ty l)) 
        | _ -> type_error (sprintf "expecting an arrow on left side of application but got %s" (pretty_ty t1))

    | Let (x, e1, e2) ->
        let t1 = typecheck_expr env e1
        typecheck_expr ((x, t1) :: env) e2

    | IfThenElse (e1, e2, Some e3) ->
        let t1 = typecheck_expr env e1
        if t1 <> TyName "bool" then type_error (sprintf "if condition must be a bool but got a %s" (pretty_ty t1))
        let t2 = typecheck_expr env e2
        let t3 = typecheck_expr env e3
        if t2 <> t3 then type_error (sprintf "type mismatch in then (%s) and else (%s)" (pretty_ty t2) (pretty_ty t3))
        t2

    | Tuple es ->
        TyTuple (List.map (typecheck_expr env) es)
        
    | LetRec (f, tf, e1, e2) ->
        let env0 = (f, tf) :: env
        let t1 = typecheck_expr env0 e1
        match t1 with
        | TyArrow _ -> ()
        | _ -> failwithf "typecheck_expr: letrec is restricted to functions but got type %s" (pretty_ty t1)
        if t1 <> tf then failwithf "typecheck_expr: letrec type mismatch: expected %s but got %s" (pretty_ty tf) (pretty_ty t1)
        typecheck_expr env0 e2

// mind that from Lesson 21 on this code has migrated into a separate project: TinyML
