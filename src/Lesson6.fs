module Lesson6

// this is another famous higher-order function: it sums anything given a plus operator and a zero element
// sum : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
let rec sum (+) zero l =
    match l with
    | [] -> zero
    | x :: xs -> x + sum (+) zero xs

// this is a monomorphic version of the function above just for integers
let rec sum_int l =    // sum_int : int list -> int
    match l with
    | [] -> 0
    | x :: xs -> x + sum_int xs

// actually you don't need to replicate the implementation for having a monomorphic version for integers: you can just use currying
let sum_int' = sum (+) 0    // sum_int' : int list -> int
// since 0 has type int, the compiler infers the correct overload of the global (+) operator

// this ETA EXPANSION is unneeded: the binding just above is equivalent
let sum_int'' l = sum (+) 0 l
// eta expansion is defined as a syntatic transformation that preserves the semantics:
// given an expression E of type T -> S, where T and S are some types, then the lambda expression fun x -> E x is the eta expansion of E and has the same type of E, i.e. T -> S
// mind that the type of E must have at least 1 arrow, or application would not be possible; in other words E must be some function value


// this is another important higher-order function: it removes from a given list l all the elements that do not satisfy the given predicate p
// mind that in F# and other functional languages à la ML no side effects occurs: the input list is not modified and NEW list is constructed recursively OMITTING the elements that do not satisfy the predicate
let rec filter p l =    // filter : ('a -> bool) -> 'a list -> 'a list
    match l with
    | [] -> []
    | x :: xs ->
        let t = filter p xs
        if p x then x :: t else t

// these are tests of the sum functions with different types
let test_sum () =
    let r1 = sum (+) 0 [1 .. 10]
    let r2 = sum (+) 0.0 [1.; 2.; 3.]
    let r3 = sum (+) "" ["ciao"; "sono"; "io"]
    let r4 = sum (fun x y -> x + " " + y) "" ["ciao"; "sono"; "io"]
    let r5 = sum (fun x y -> sprintf "%s %s" x y) "" ["ciao"; "sono"; "io"]
    let r6 = sum (sprintf "%s %s") "" ["ciao"; "sono"; "io"]
    printfn "r1 = %d" r1


