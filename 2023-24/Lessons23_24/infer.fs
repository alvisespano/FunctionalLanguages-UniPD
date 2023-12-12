module infer

exception UnificationError of string

type ty = Name of string
        | Arrow of ty * ty
        | Var of int 

(*public abstract class Ty {
    public abstract String pretty(); 
    public abstract Subst unify(Ty t2);
}

public class Name extends Ty {
    public String s;

    @Override
    public String pretty() {
        return s;
    }
}

public class Arrow extends Ty {
    public Ty domain, comain;

    @Override
    public String pretty() {
        return domain.pretty() + " -> " codomain.pretty();
    }
}

public class Var extends Ty {
    public int n;

    @Override 
    public String pretty() {
        return "'" + n;
    }
}*)
(*with   
    override self.ToString () =
        match self with
        | Name s -> s
        | Arrow (t1, t2) -> sprintf "%O -> %O" t1 t2
        | Var n -> sprintf "'%d" n*)

let rec pretty_ty t =
    match t with
    | Name s -> s
    | Arrow (t1, t2) -> sprintf "%s -> %s" (pretty_ty t1) (pretty_ty t2)
    | Var n -> sprintf "'%d" n


type subst = (int * ty) list

let compose_subst s1 s2 = [] // TODO

let rec apply_subst s t = 
    match t with
    | Name _ -> t
    | Arrow (t1, t2) -> Arrow (apply_subst s t1, apply_subst s t2)
    | Var n -> let _, r = List.find (fun (n', t) -> n = n') s in r

let rec unify t1 t2 =
    match (t1, t2) with
    | (Name s1, Name s2) when s1 = s2 -> []
    | (Var n, t)
    | (t, Var n) -> [n, t]
    | (Arrow (t1, t2), Arrow (t3, t4)) -> compose_subst (unify t1 t3) (unify t2 t4) 
    | _ -> raise (UnificationError (sprintf "%s does not unify with %s" (pretty_ty t1) (pretty_ty t2))



