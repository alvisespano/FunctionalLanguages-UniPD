module Lesson1

// this is a simple function picking an integer and returning its successor
let plus1 n = n + 1     // plus1 : int -> int
// in C language it can be written as:
//      int plus1(int n) { return n + 1; }


// this is a tuple
let mypair = (4, "ciao")    // mypair : int * string
// this cannot be written in C because there are no tuples in the language

// this is a function picking a pair and summing the two components: it infers integer because of the (+) operator
let plus_pair (a, b) = a + b    // plus_pair : int * int -> int
// in C language there are no pairs but something almost equivalent could be simply written as:
//      int plus_pair(int n, int m) { return n + m; }

// this is a function picking a triple: f is a function, x and y are value of any type
// parametric polymorphism allows the definition of functions operating on values of any type
// 'a, 'b and such are TYPE VARIABLES automatically inferred by the compiler
// 'a means "some unknown type"; 'b means "some other unknown type"
// when a given type variable occurs more than once in a type it means it is the SAME unknown type
let map_pair (f, x, y) = (f x, f y) // map_pair : ('a -> 'b) * 'a * 'a -> 'b * 'b
// in this case f : 'a -> 'b, which means it is a function picking something of an unknown type 'a and returning something else of unknown type 'b
// the type of x and y is 'a, which means that both x and y have the SAME unknown type, which is also the same type that function f picks as argument
// the overall result of map_pair is a pair of type 'b * 'b, where 'b is an unknown type that could possibly be different from 'a and that is the return type of function f
// this is how parametric polymorphism works: unknown types become polymorphism type variables that have a NAME ('a, 'b, 'c etc) and may occur multiple times in a type signature


// this is the identity function, which is polymorphic: it picks something of any type and return the same thing of the SAME any time
let identity x = x  // identity : 'a -> 'a
// in Java that can be achieved in 2 ways:
// using subtyping polymorphism:
//      public static Object ident(Object x) { return x; }
// or using generics, which are basically equivalent to parametric polymorphism, though without type inference, hence with type variables named by the programmer:
//      public static <T> T ident(T x) { return x; }
// parametric polymorphism (a.k.a. generics) is superior to subtyping, because you do not lose type information and do not need to typecast the result

// in C++ you could use the template system for achieving parametric polymorphism (a.k.a. generics):
// template <typename T> T identity(T x) { return x; }
// that is equivalent to Java generics but with a subtle different: the C++ compiler does not compile templatized functions until instantiated with a concrete type
// this makes the C++ template system a sophisticate form of automatic macro expansion based on syntactic substitution of type names
