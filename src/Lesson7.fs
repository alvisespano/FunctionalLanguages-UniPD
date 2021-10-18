module Lesson7

// fold : f:('a -> 'b -> 'a) -> z:'a -> l:'b list -> 'a
let rec fold_right f z l = 
    match l with
    | [] -> z
    | x :: xs -> f (fold_right f z xs) x

let rec fold_left f z l = 
    match l with
    | [] -> z
    | x :: xs -> fold_left f (f z x) xs

let fold_imp f z l =
    let mutable l = l
    let mutable z = z
    while not (List.isEmpty l) do
        let x = List.head l
        z <- f z x 
        l <- List.tail l
    z

// map : ('a -> 'b) -> 'a list -> 'b list
let rec map f l =
    match l with
    | [] -> []
    | x :: xs -> f x :: map f xs


let map2 f l = fold_left (fun z x -> z @ [f x]) [] l

let list_length l = fold_left (fun z _ -> z + 1) 0 l

let test_fold () =
    let r1 = fold_left (fun z s -> sprintf "%s, %s" s z) "" ["ciao"; "it"; "is"; "me"]
    let r2 = fold_left (fun z x -> x + z) 0 [1 .. 20]
    let r3 = list_length [1 .. 23]
    let r4 = map (fun x -> printf "%d " x; x + 1) [1 .. 5]
    let r5 = map2 (fun x -> printf "%d " x; x + 1) [1 .. 5]
    printf "\n%O\n%O\n" r4 r5