module Lesson17

type lit = LInt of int
         | LFloat of float
         | LString of string
         | LUnit 

type expr = 
    | Lit of lit
    | Lambda of string * expr
    | App of expr * expr
    | Var of string
    | Let of string * expr * expr
    | LetRec of string * string * expr * expr
    | IfThenElse of expr * expr * expr option
    
type 'a env = string * 'a list  

type value =
    | VLit of lit
    | Closure of value env * string * expr
    | RecClosure of value env * string * string * expr

type tyvar = int

type ty =
    | TyName of string
    | TyArrow of ty * ty
    | TyVar of tyvar

type scheme = Forall of tyvar list * ty