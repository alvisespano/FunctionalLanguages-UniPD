module Lesson6

let rec sum_int l =    // sum : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
    match l with
    | [] -> 0
    | x :: xs -> x + sum_int xs


// this is another higher-order function: it sums anything given a plus operator and a zero element
// sum : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
let rec sum (+) zero l =
    match l with
    | [] -> zero
    | x :: xs -> x + sum (+) zero xs

// filter : ('a -> bool) -> 'a list -> 'a list
let rec filter p l =
    match l with
    | [] -> []
    | x :: xs ->
        let t = filter p xs
        if p x then x :: t else t

// TODO MONDAY
//let rec fold f z l = ...

let test_sum () =
    let r1 = sum (+) 0 [1 .. 10]
    let r2 = sum (+) 0.0 [1.; 2.; 3.]
    let r3 = sum (+) "" ["ciao"; "sono"; "io"]
    let r4 = sum (fun x y -> x + " " + y) "" ["ciao"; "sono"; "io"]
    let r5 = sum (fun x y -> sprintf "%s %s" x y) "" ["ciao"; "sono"; "io"]
    let r6 = sum (sprintf "%s %s") "" ["ciao"; "sono"; "io"]
    printfn "r1 = %d" r1