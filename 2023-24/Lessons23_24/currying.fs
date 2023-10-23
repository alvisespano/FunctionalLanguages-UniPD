module currying



// curry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
let curry f (x, y) = f x y

// uncurry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
let uncurry f x y = f (x, y)

////////////////////////

// addition : int -> (int -> int)
let addition = fun x -> fun y -> x + y

// addition2 : int * int -> int
let addition2 = fun (x, y) -> x + y

let z = addition 7 // z : int -> int

let z2 = addition2 (7, 8)

let eight = z 1

/////////////////////////

// pow : int -> _ -> _
let rec pow bas exp =
    if exp > 1 then bas * (pow bas (exp - 1))
    else 1

let rec pow2 (bas, exp) = 
    if exp > 1 then bas * (pow2 (bas, exp - 1))
    else 1













