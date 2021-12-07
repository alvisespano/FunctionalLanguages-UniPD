module Lesson17

type tyvar = int

type ty =
    | TyName of string
    | TyArrow of ty * ty
    | TyVar of tyvar

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
    | LetRec of string * string * expr * expr
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

    | LetRec (f, x, e1, e2) ->
        sprintf "let rec %s %s = %s in %s" f x (pretty_expr e1) (pretty_expr e2)

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

let rec typecheck_expr (env : ty env) e =
    match e with
    | Lit (LInt _) -> TyName "int"
    | Lit (LFloat n) -> TyName "float"
    | Lit (LString s) -> TyName "string"
    | Lit LUnit -> TyName "unit"

    | Lambda (x, None, e) -> failwith "typecheck_expr: unannotated lambdas are not supported"
    
    | Lambda (x, Some t1, e) ->
        let t2 = typecheck_expr ((x, t1) :: env) e
        TyArrow (t1, t2)
