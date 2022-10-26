
module FL2022_23.Lesson1

///////////////////////////////
// bindings and values
///////////////////////////////

let k = 4

let f x = x + 1

let g (x, y) = x + 1 + y

let h (x, y, z) = if x && y > 2 then y else z

let j (x, y) = y (x + 1) + 1

let u (a, b, c) = if a (b c) then c else 3

let i x = x

let p1 = (i 4, i true, i (if i 5 > 6 then i 4 else i (i 3)))

let swap (x, y) = (y, x)

// swap2: 'a * 'a * ('a -> 'b) -> 'b * 'b
let swap2 (x, y, f) = (f y, f x)


let app1 (f, x) = f (x + 1)

















