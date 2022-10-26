
module FL2022_23.Lesson2

////////////////////////////////
// lexical static scoping and closures
////////////////////////////////

let static_scoping_example () =
    let k = 4

    // a lambda expression produces a CLOSURE: a closure stores 3 information: the lambda parameter name (x in this case), the lambda body (x + k) and the current scope
    // so the scope is stored together with the lamdba, to allow application occur in the original scope
    let f = fun x -> x + k      //  variable k refers to 4

    // we rebind k
    let k = 5

    // and then we apply function f to an argument 8
    in f 8     // evaluates 8 + 4, not 8 + 5, because the k appearing in the lambda carries along its original scope 


////////////////////////////////
// syntactic sugar for function let-bindings
////////////////////////////////

let g (x, y, z) = (x + 1, y + z)

// the g above de-sugars to this
let g2 = fun (x, y, z) -> (x + 1, y + z)












