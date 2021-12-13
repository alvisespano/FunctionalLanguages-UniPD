module TinyML.Ast

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
         | LChar of char
         | LBool of bool
         | LUnit 

type expr = 
    | Lit of lit
    | Lambda of string * ty option * expr
    | App of expr * expr
    | Var of string
    | Let of string * ty option * expr * expr
    | LetRec of string * ty option * expr * expr
    | IfThenElse of expr * expr * expr option
    | Tuple of expr list
    | Plus of expr * expr
    | Minus of expr * expr
    | Times of expr * expr
    | Div of expr * expr
    | Mod of expr * expr
    | Eq of expr * expr
    | Neq of expr * expr
    | Gt of expr * expr
    | Geq of expr * expr
    | Lt of expr * expr
    | Leq of expr * expr
    | UMinus of expr
    | Not of expr

    
    
type 'a env = (string * 'a) list  

type value =
    | VLit of lit
    | VTuple of value list
    | Closure of value env * string * expr
    | RecClosure of value env * string * string * expr


let rec flatten f es =
    match es with
    | [] -> ""
    | [e] -> f e
    | e :: es -> sprintf "%s, %s" (f e) (flatten f es)

let rec pretty_ty t =
    match t with
    | TyName s -> s
    | TyArrow (t1, t2) -> sprintf "%s -> %s" (pretty_ty t1) (pretty_ty t2)
    | TyVar n -> sprintf "'%d" n
    | TyTuple ts -> sprintf "(%s)" (flatten pretty_ty ts)

let rec pretty_expr e =
    match e with
    | Lit (LInt n) -> sprintf "%d" n
    | Lit (LFloat n) -> sprintf "%g" n
    | Lit (LString s) -> s
    | Lit (LChar c) -> sprintf "%c" c
    | Lit (LBool true) -> "true"
    | Lit (LBool false) -> "false"
    | Lit LUnit -> "()"
    
    | Lambda (x, None, e) -> sprintf "fun %s -> %s" x (pretty_expr e)
    | Lambda (x, Some t, e) -> sprintf "fun (%s : %s) -> %s" x (pretty_ty t) (pretty_expr e)
    
    // TODO pattern-match sub-application cases
    | App (e1, e2) -> sprintf "%s %s" (pretty_expr e1) (pretty_expr e2)

    | Var x -> x

    | Let (x, None, e1, e2) ->
        sprintf "let %s = %s in %s" x (pretty_expr e1) (pretty_expr e2)

    | Let (x, Some t, e1, e2) ->
        sprintf "let %s : %s = %s in %s" x (pretty_ty t) (pretty_expr e1) (pretty_expr e2)

    | LetRec (x, None, e1, e2) ->
        sprintf "let rec %s = %s in %s" x (pretty_expr e1) (pretty_expr e2)

    | LetRec (x, Some tx, e1, e2) ->
        sprintf "let rec %s : %s = %s in %s" x (pretty_ty tx) (pretty_expr e1) (pretty_expr e2)

    | IfThenElse (e1, e2, e3o) ->
        let s = sprintf "if %s then %s" (pretty_expr e1) (pretty_expr e2)
        match e3o with
        | None -> s
        | Some e3 -> sprintf "%s else %s" s (pretty_expr e3)
        
    | Tuple es ->        
        sprintf "(%s)" (flatten pretty_expr es)
