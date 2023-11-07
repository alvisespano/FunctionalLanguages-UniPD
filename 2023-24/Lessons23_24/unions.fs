module unions



type baudo = int


type person = { name : string; surname : string; age : int }


type color = Black | Yellow | White | Red | Rgb of int * int * int

let col1 = Rgb

let make_brigher c =
    match c with
    | Black  -> White
    | Red    -> Yellow
    | White  -> White
    | Yellow -> Yellow
    | Rgb (r, g, b) -> Rgb (r * 2, g * 2, b * 2)



type mylist_ugly = { data : int; next : mylist }

let l1 = { data = 1; next = { data = 2 ; next = { data = 3 ; next = }}}


type mylist = Empty | NonEmpty of int * mylist

let l2 = Empty

let l3 = NonEmpty (1, NonEmpty (2, Empty))




//type 'a list = [] | (::) of 'a * 'a list

let l4 = 1 :: 45 :: 765 :: 3 :: []

let l5 = [1; 45; 765; 3]

let l6 = []

let l7 = ["ciao"; "sono"; "io"]

let l8 = [(1, true); (5, false)]

let l9 = [[1]; []; [1; 2; 3]]

let l10 = ([[[1]]], [fun x -> x; fun x -> x + 1])






