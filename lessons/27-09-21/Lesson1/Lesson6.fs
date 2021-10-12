module Lesson6

// this is another higher-order function: it sums anything given a plus operator and a zero element
let rec sum (+) zero l =    // sum : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
    match l with
    | [] -> zero
    | x :: xs -> x + sum (+) zero xs

