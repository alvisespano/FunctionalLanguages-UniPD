module Lesson2

// this is a UNION type (a.k.a. variant or disjoint union)
// it is a type that has multiple values in a similar way as an enum in C or other language: it means that a color can EITHER be Red, or Yellow, or Black etc.
type color = Red | Yellow | Black | White | Green | Rgb of int * int * int
// the last data constructor is parametric: it means that it not a constant value but it is like a function picking a triple of ints and producing a color
// this is very profound: we can define types by cases and some cases can carry data along with them, so that's different from plain enumerations in C or Java
// this enables the construction of data structures that can eventually be dismounted/deconstructed through pattern matching

// this is a value of type color
let color1 = Red
// this is a tuple of type color * color * float * string
let tup1 = (White, Black, 3.5, "ciao")
// this is a function of type int -> color
let myfun1 x = if x < 0 then Red else Black
// also this is a value of type color
let color2 = Rgb (255, 255, 255)
// this is basically an alias of the data construct Rgb, having the same type it has: int * int * int -> color
let rgb = Rgb

// this is an example of pattern matching
let convcol c =
    match c with
    | Red -> 0
    | Yellow -> 1
    | Rgb (r, g, b) -> r + g + b    // this is the interesting case: you BIND names r, g and b to the 3 integers attached to Rgb and you can use them on the right side
    | _ -> 11   // this is the fallback case

(*
How would you do that in C?

enum color { RED, YELLOW, BLACK, WHITE, BLUE, GREEN };

We CANNOT encode the Rgb case though, because C and other imperative language do not allow enum cases with data attached to it

This would be a simple translation to C, but no Rgb case is supported - and that's the interesting part!
void convcol(color x) {
    switch(x) {
    case Red: return 0;
    case Yellow: return 1;
    default: return 11;
}
*)