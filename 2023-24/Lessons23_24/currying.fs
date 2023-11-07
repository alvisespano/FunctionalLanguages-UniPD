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

let x = 9

let rec f x =
    let f x = x + 1
    let rec f x = f (x + 1)
    f x

let rec r () = r () + 1

// pow : int -> int -> int
let rec pow bas exp =
    if exp > 1 then bas * (pow bas (exp - 1))
    else 1

// pow2 : int * int -> int
let rec pow2 (bas, exp) = 
    if exp > 1 then bas * (pow2 (bas, exp - 1))
    else 1


////////////////////////////

(*public class Animal {
    protected int weight;
    
    public Animal(int w) { this.weight = w; }

    public void eat(Animal a) { this.weight += a.weight; }
}

public class Dog extends Animal {
    public Dog(int w) { super(w); }
    @Override
    public void eat(Animal a) { this.eat(this); } 
}

public static void main() {
    Animal poppy = new Dog(10);
    Dog toby = new Dog(20);
    poppy.eat(toby);
}*)


(*
struct S {
   int a;
   double b;
   char** c;
}

union U {
   int n;
   char bytes[4];
}

enum Color { BLACK, RED, WHITE, YELLOW };
*)
















