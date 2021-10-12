module Lesson5

open Lesson3

let test_currying1 () =
    let g = iter (fun x -> printf "%d" x)   // this is currying
    g [1; 2; 3]
    g [5; 6]

// also the printf is a curried function, so you can exploit currying to perform partial application of arguments
let test_currying_printf () =
    let p = printf "ciao i have %d years and my name is %s" // the printf is also TYPED in F#
    p 7 "alvise"

let test_currying2 () =
    let m1 = map (printf "%d")  // two curryings in there
    m1 [1; 2; 3] 

// this is another example using currying and operators
let test_app () =
    let app2 f x y = f x y  // this applies f to x and y in curried form
    let n = app2 (+) 2 3
    let m = app2 (/) 20 2
    let plus = app2 (+)
    let r1 = plus 7 8
    let m1 = app2 map ((+) 1) [1; 2; 3]
    ()

// this currifies a function
let currify f (a, b) = f a b
// this uncurrifies a function
let uncurrify f a b = f (a, b)



