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
    | Tuple of expr list
    
type 'a env = string * 'a list  

type value =
    | VLit of lit
    | VTuple of value list
    | Closure of value env * string * expr
    | RecClosure of value env * string * string * expr

type tyvar = int

type ty =
    | TyName of string
    | TyArrow of ty * ty
    | TyVar of tyvar

type scheme = Forall of tyvar list * ty

let rec pretty_expr e =
    match e with
    | Lit (LInt n) -> sprintf "%d" n
    | Lit (LFloat n) -> sprintf "%g" n
    | Lit (LString s) -> s
    | Lit LUnit -> "()"
    
    | Lambda (x, e) -> sprintf "fun %s -> %s" x (pretty_expr e)
    
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
        sprintf "(%s)"  // TOCO fold list es to produce string separated with commas
        

(*
public class Animal {
    protected int age;
    public Animal(int age) { this.age = age; }
}

public class Dog extends Animal {
    private String hairColor;
    public Dog(int age, String hairColor) {
        this(age);
        this.hairColor = hairColor;
    }
}

public static int main(String[] args) {
    Animal fido = new Dog(12, "maculato");
    
    return 0;
}

*)