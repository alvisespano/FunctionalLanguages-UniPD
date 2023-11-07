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
                