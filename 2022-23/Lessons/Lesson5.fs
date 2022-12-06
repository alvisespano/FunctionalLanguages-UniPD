module Lesson5


let f a b c = if a then b c else a 

let g x y z = (x - 1, y, z)

let h x y z = z y x

let id x = x

let myfun1 x y =
    let f z = f x y x
    let k = 3
    f k

let j x = if x then x else x

let u = (fun x -> x) 3

let w = (1 + 2 + 3) 4   // left part of app is wrong


module Tests =

    let k = 1
    
    let f = fun x -> x + k

    let k = 8

    let n = f 2





