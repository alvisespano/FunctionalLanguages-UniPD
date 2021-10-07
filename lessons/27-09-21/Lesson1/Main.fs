
// int plus1(int n) { return n + 1; }
let plus1 n = n + 1 // plus1 : int -> int

let mypair = (4, "ciao")

let plus_pair (a, b) = a + b

let map_pair (f, x, y) = (f x, f y)
// map_pair : ('a -> 'b) * 'a * 'a -> 'b * 'b



(*public static Object ident(Object x) { return x; }
public static <T> T ident2(T x) { return x; }*)
let identity x = x  // identity : 'a -> 'a

// map : f:('a -> 'b) * l:'a list -> 'b list
let rec map (f, l) =
    match l with
    | [] -> []
    | x :: xs -> f x :: map (f, xs)

let test_map () =
    let l1 = ["ciao"; "my"; "name"; "is"; "alvise"]
    let r1 = map ((fun (s : string) -> s.Length), l1)
    printf "l1 = %O\nr1 = %O\n" l1 r1

(*type 'a mylist = Empty | NonEmpty of 'a * 'a mylist

type 'a list = [] | :: of 'a * 'a list

let l1 : int mylist = NonEmpty (2, NonEmpty (3, Empty))
let l2 = 2 :: 3 :: []
let l3 = [2; 3]

void myswitch(int x) {
    if (x == 0) blabla1
    else if (x == 1) blabla2
    else if (x == 2) blabla3
    else blabla4

    switch(x) {
    case 0: blabla1
    case 1: blabla2
    case 2: blabla3
    default: blabla4
    }

enum Color { RED, YELLOW, BLACK, WHITE, BLUE, GREEN };

let myswitch x =
    match x with
    | 0 -> blabla1
    | 1 -> blabla2
    | 2 -> blasbla3
    | _ -> blabla4
 
}
*)

type color = Red | Yellow | Black
           | White | Green | Rgb of int * int * int

let color1 = Red
let tup1 = (White, Black, 3.5, "ciao")
let myfun1 x = if x < 0 then Red else Black
let color2 = Rgb (255, 255, 255)

let convcol c =
    match c with
    | Red -> 0
    | Yellow -> 1
    | Rgb (r, g, b) -> r + g + b
    | _ -> 11


(*

int add(int x, int y) {
   return x + y;
}

double add(double x, double y) {
    return x + y;
}

template <typename T>
T add(T x, T y) {
    return x + y;
}

object identity(object x) {
    return x;
}

template <typename T>
T identity2(T x) { return x; }

void main() {
    object a = identity(3);
    double x = identity(4.5);
}

*)

[<EntryPoint>]
let main argv =
    test_map ()
    0
   
    