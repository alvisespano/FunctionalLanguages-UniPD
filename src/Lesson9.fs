module Lesson9

module Tmp =
    let f1 x = x + 1

    let f2 x = x - 2

    let f3 x y = x + y

    let main () =
        f3 (f1 7) (f2 8)

////////

let f1 = fun x -> (+) x 1 in
let f2 = fun x -> (-) x 2 in
let f3 = fun x -> fun y -> (+) x y in
let main = fun () -> f3 (f1 7) (f2 8) in
main ()

////////

(*
int main() {
    int n = 3;
    retu n - 2;
}
*)

///////////

let h () =
    let v = (let f = fun x -> x + 8 in f) 3
    let v2 = (let n = 1 in n) 7
    ()

(*
int g() {
    int n = 8;
    return n;
}
*)

let g = fun () -> (let n = 8 in n) + 1



