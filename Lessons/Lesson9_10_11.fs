module Lesson9_10_11
// this source file includes 3 lessons because very few code was produced during classes and most of the time the blackboard was used
// see the video lessons 9, 10 and 11 for details


// this series of bindings inside a module is like a standalone F# source file: multiple let-bindings with eventually one main at the end
// the reason why these are in a module is for avoiding name clashing
// consider that a source file is itself a module, whose name is the name of the module at the first line: in this source file the name of the module is Lesson9_10_11 (see line 1)
module Program =
    let f1 x = x + 1

    let f2 x = x - 2

    let f3 x y = x + y

    let main () =
        f3 (f1 7) (f2 8)

// the program contained in the module above can be translated into one single expression as follows
// in other words, any series of global let-bindings can be turned into one big expression nesting those lets and eventually calling the main function
let program =
    let f1 = fun x -> (+) x 1 in
    let f2 = fun x -> (-) x 2 in
    let f3 = fun x -> fun y -> (+) x y in
    let main = fun () -> f3 (f1 7) (f2 8) in
    main () // this is the final body of this big series of nested lets: it is an application, simply calling function main


// now consider the following expressions: these are sample applications using let..in for creating local bindings

let v1 = (let f = fun x -> x + 8 in f) 3    // this is an application where the left expression is a let
// v1 : int = 11

let v2 = (let n = fun x -> x in n) (let n = 7 in n - 1) // this is an application too, but let is used locally with the same name: no clash occurs
// v2 : int = 6

let v3 = (let n = 8 in n) + 1      // this is a call to the plus (+) operator where the left operand is a let
// v3 : int = 9

// what is important to understand is that the let..in construct can be used ANYWHERE, nested within any expression and allows the programmer to create
// new bindings that are just local to the expression following the in keyword: the whole let..in expression evaluates to the expression after the in, which is basically the body


let prg1 =
    let y = 6 
    let f = fun x -> x + y
    let y = 9
    let g = 11
    let z = f 8
    //let w = g 9
    z

type anything = Me | You

let prg2 =
    let f = 5
    let f = fun x -> x + 1
    let f2 = fun x -> x // f2 : 'a -> 'a
    let h = f2 6, f2 "ciao", f2 5.6
    let f3 = fun (x, y) -> (y, x)   // f3 : 'a * 'b -> 'b * 'a
    let f4 = fun x -> fun y -> x y  // f4 : ('a -> 'b) -> 'a -> 'b
    let rec g = fun (x : int) -> if x < 0 then "ciao" else g (x + 1)
    f 8

let prg3 =
    let k = 1
    let f = fun x -> x + k
    let k = 8
    f 9

type person = { name : string; surname : string; age : int }

let alvise = { name = "Alvise"; surname = "Spano'"; age = 44 }

let prg4 =
    let f = fun x -> fun y ->
        let g = fun z -> (z, y)
        if y = 7 then 0 else 1
    let r1 = f true 9
    let r2 = f "ciao" 8
    ()


let rec f = fun x -> if x < 0 then 0 else f (x - 1)

let rec id = fun x -> id x 
id 8




(*
int M[10];

void sum_array(int x) {
    int r = 0;
    for (int i = 0; i < x; ++i)
        r += M[i];
    return r;
}


*)


(*
int i = 5;
string s = "ciao";

i = s;
int j = i + 4;

*)

