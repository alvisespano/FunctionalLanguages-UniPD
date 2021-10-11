
// int plus1(int n) { return n + 1; }
let plus1 n = n + 1 // plus1 : int -> int

let mypair = (4, "ciao")

let plus_pair (a, b) = a + b

let map_pair (f, x, y) = (f x, f y)
// map_pair : ('a -> 'b) * 'a * 'a -> 'b * 'b



(*public static Object ident(Object x) { return x; }
public static <T> T ident2(T x) { return x; }*)
let identity x = x  // identity : 'a -> 'a

// map : f:('a -> 'b) -> l:'a list -> 'b list
let rec map f l = // fun f -> fun l -> ...
    match l with
    | [] -> []
    | x :: xs -> f x :: map f xs

// iter : ('a -> unit) -> 'a list -> unit
let rec iter =
    fun f ->
        fun l -> 
            match l with
            | [] -> ()
            | x :: xs -> f x; iter f xs

// void printf(const char* fmt, ...)
(*
printf("ciao i have %d years and %d months", 8, "pippo", true);
*)


let prova () =
    let g = iter (fun x -> printf "%d" x)
    g [1; 2; 3]
    g [5; 6]
    let p = printf "ciao i have %d years and my name is %s"
    p 7 "alvise"

let prova2 () =
    let m1 = map (printf "%d")
    m1 [1; 2; 3] 

let app2 f x y = f x y

let prova3 () =
    let n = app2 (+) 2 3
    let m = app2 (/) 20 2
    let plus = app2 (+)
    let r1 = plus 7 8
    let m1 = app2 map ((+) 1) [1; 2; 3]
    ()

let rec sum (+) zero l =
    match l with
    | [] -> zero
    | x :: xs -> x + sum (+) zero xs

let currify f (a, b) = f a b
let uncurrify f a b = f (a, b)

let add x y = x + y
let u = add 7

let test_map1 () =
    let l1 = ["ciao"; "my"; "name"; "is"; "alvise"]
    let f1 = fun (s : string) -> s.Length
    let f2 (s : string) = s.Length
    let r1 = map f1 l1
    printf "l1 = %O\nr1 = %O\n" l1 r1

let test_map2 () =
    let l1 = [1.2; 23.45; 56.78]
    let r1 = map (fun n -> n + 1.0) l1
    printf "l1 = %O\nr1 = %O\n" l1 r1


let swap (x, y) = (y, x)
let test_map3 () =
    let l1 = [(4, true); (6, false); (88, true)]
    let r1 = map swap l1
    printf "l1 = %O\nr1 = %O\n" l1 r1

let test_map4 () =
    let l1 = [1.2; 23.45; 56.78]
    let r1 = map (fun n -> printf "%f" n) l1
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
   
    