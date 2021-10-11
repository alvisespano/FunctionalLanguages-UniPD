module Lesson3



// this is a famous higher-order function, available on the standard library as well: it picks a function f and a polymorphic list l, applies f to all the elements of l and returns the list with all the results
// it basically transforms a list of something into a list of something else through a conversion function
let rec map f l = // map : ('a -> 'b) -> 'a list -> 'b list
    match l with
    | [] -> []
    | x :: xs -> f x :: map f xs
// this is in curried form, hence with non-tupled parameters

// mind that the syntax for binding functions specifying parameters after the function name and BEFORE the equal is just SYNTAX SUGAR
// which means that it is not a native construct of the language, but only a shortcut for some other syntax
// in the case of map, it would be desugared to:
let rec map_desugar =
    fun f ->
        fun l ->
            match l with
            | [] -> []
            | x :: xs -> f x :: map_desugar f xs
// this is absolutely identical to the form above: having 2 parameters actually means to have 2 nested lambdas
// that's why CURRYING works: because nested lambdas produce nested arrows and require multiple application in the same order when calling the function

// its non-curried form would be the following: basically the difference is that the 2 parameters are in tuple form rather than in multiple arrows
let rec map_uncurried (f, l) = // map_uncurried : ('a -> 'b) * 'a list -> 'b list
    match l with
    | [] -> []
    | x :: xs -> f x :: map_uncurried (f, xs)
// usually, currying is preferable because you don't need tupled arguments and because you can exploit partial application




// this is a variant of the map function aimed at performing side-effects for each element of a list
// it differs from the map higher-order function because it does not produce a result list: it only applied function f every element of l. Just that.
// so, in other words, since it has no result, it means that f must be a function that performs some side-fx only
let rec iter f l =  // iter : ('a -> unit) -> 'a list -> unit
    match l with
    | [] -> ()
    | x :: xs -> f x; iter f xs // the semicolon operator wants an expression of type unit on the left and an expression of any type on the right, the overall result being the right part


// now a number of test functions and sample calls follow

let test_map1 () =
    let l1 = ["ciao"; "my"; "name"; "is"; "alvise"]
    let f1 = fun (s : string) -> s.Length
    let f2 (s : string) = s.Length
    let r1 = map f1 l1
    printf "l1 = %O\nr1 = %O\n" l1 r1

let test_map2 () =
    let l1 = [1.2; 23.45; 56.78]
    let r1 = map (fun n -> n + 1.0) l1
    printf "l1 = %O\nr1 = %O\n" l1 r1


let swap (x, y) = (y, x)
let test_map3 () =
    let l1 = [(4, true); (6, false); (88, true)]
    let r1 = map swap l1
    printf "l1 = %O\nr1 = %O\n" l1 r1

let test_map4 () =
    let l1 = [1.2; 23.45; 56.78]
    let r1 = map (fun n -> printf "%f" n) l1
    printf "l1 = %O\nr1 = %O\n" l1 r1