module Main


let x = 7

let y = "ciao"

let (a, b) = (1, true)

let f x = x + 1 // f : int -> int

let g x = if x then 1 else 2  // g : bool -> int


(*
IMPERATIVE
----------
int i = 0;
while (i < 10) {
    i = i + 1;      // assigment = imperative
}

RECURSIVE
---------
void recfun(int i) {
    if (i < 10) recfun(i + 1);  // recursion does not modify anything
}
*)

let rec recfun i = 
    if i < 10 then
        recfun (i + 1)

(* 
int fib(int x) {
    if (x < 2) return 1;
    else return fib(x - 1) + fib(x - 2);
}
*)

let rec fib x =
    if x < 2 then 1
    else fib (x - 2) + fib (x - 1)

(*
int fact(int x) {
    return x < 2 ? 1 : x * fact(x - 1);
}
*)

let rec fact x = 
    if x < 2 then 1 else x * fact(x - 1)


//////////////////////////////

let swap (a, b) = (b, a)

let (p, q) = swap (1, 56)
let (r, t) = swap (true, "ciao")
let (v, w) = swap ((4.5, "ciccio"), 7)

(*
public class Pair<A, B> {
    public A first;
    public B second;
}

public class Swap {
    public static <A, B> Pair<B, A> swap(Pair<A, B> p) {
        return new Pair(p.second, p.first);
    }
}
*)


(*
JAVA
----
public static Object ident(Object x) { return x; }
public static <T> T ident(T x) { return x; }

public static <A extends String, B extends Number> A f(A a, B b) { ..... }
-----> String f(String a, Number b)
...

Object a = ident(new ArrayList<Integer>());


C
-
int ident_int(int x) { return x; }
char* ident_charpt(char* x) { return x; }
void* ident_voidpt(void* x) { return x; }
*)

let ident x = x

let ident2 = fun x -> x

// ident(x) = x



let swap2 (x, y) = (y, x)



fun x -> fun y -> x * 2 + y



