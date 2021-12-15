module Lesson7

// fold_right : f:('a -> 'b -> 'a) -> z:'a -> l:'b list -> 'a
let rec fold_right f z l = 
    match l with
    | [] -> z
    | x :: xs -> f (fold_right f z xs) x    // non tail recursive

// fold_left : f:('a -> 'b -> 'a) -> z:'a -> l:'b list -> 'a
let rec fold_left f z l = 
    match l with
    | [] -> z
    | x :: xs -> fold_left f (f z x) xs // tail recursive

// imperative implementation of the fold_left
let fold_left__imp f z l =
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


// map implemented though a fold_left: notice the lambda that uses tail-append rather than head-insert in order to produce the result list preserving the order
// tail-append requires the use of the (@) operator, whereas the (::) construct only inserts on the head
let map' f l = fold_left (fun z x -> z @ [f x]) [] l

// calculate the length of the given list l using fold_left for counting
// the underscore (_) as second lambda parameter means that we ignore it: the parameters still exists and the lambda is still a binary function, but we do not not bind a name to it
let list_length l = fold_left (fun z _ -> z + 1) 0 l

let test_fold () =
    let r1 = fold_left (fun z s -> sprintf "%s, %s" s z) "" ["ciao"; "it"; "is"; "me"]
    let r2 = fold_left (fun z x -> x + z) 0 [1 .. 20]
    let r3 = list_length [1 .. 23]
    let r4 = map (fun x -> printf "%d " x; x + 1) [1 .. 5]
    let r5 = map' (fun x -> printf "%d " x; x + 1) [1 .. 5]
    printf "\n%O\n%O\n" r4 r5

