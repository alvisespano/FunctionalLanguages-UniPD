module lists

//type 'a list = [] | (::) of 'a * 'a list

let rec length l =
    match l with
    | [] -> 0
    | _ :: t -> 1 + length t

let rec insert_tail l x =
    match l with
    | [] -> [x]
    | h :: t -> h :: insert_tail t x

// map : ('a -> 'b) -> 'a list -> 'b list
let rec map f l =
    match l with
    | []     -> []
    | h :: t -> f h :: map f t

// filter : ('a -> bool) -> 'a list -> 'a list
let rec filter p l =
    match l with
    | []     -> []
    | h :: t -> let u = filter p t
                if p h then h :: u else u
                
// iter : ('a -> unit) -> 'a list -> unit
let rec iter f l =
    match l with
    | [] -> ()
    | h :: t -> f h; iter f t

// sum_mono : int list -> int
let rec sum_mono l =
    match l with
    | [] -> 0
    | h :: t -> h + sum_mono t

// sum : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
let rec sum (+) zero l =
    match l with
    | [] -> zero
    | h :: t -> h + sum (+) zero t

// fold_back : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
let rec fold_back f accu l =
    match l with
    | [] -> accu
    | h :: t -> f h (fold_back f accu t)

// fold : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
let rec fold f accu l =
    match l with
    | [] -> accu
    | h :: t -> fold f (f h accu) t

let filter_by_fold_back p l = 
    fold_back (fun x accu -> if p x then x :: accu else accu ) [] l

let filter_by_fold p l = 
    fold (fun x accu -> if p x then accu @ [x] else accu) [] l

let map_by_fold f l =
    fold (fun x accu -> accu @ [f x]) [] l




(*
public static <A, B> B fold(BiFunction<A, B, B> f, B accu, Collection<A> c) {
    for (A x : c) {
        accu = f.apply(x, accu);
    }
    return accu;
}

public static <A, B> B recfold(BiFunction<A, B, B> f, B accu, List<A> c) {
    if (c.isEmpty()) return accu;
    A h = c.get(0)
    c.removeAt(0);
    return f.apply(h, recfold(f, accu, c));
}
*)

