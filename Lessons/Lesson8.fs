module Lesson8

open Lesson7

(*
struct tree_node {
    int data;
    struct tree_node* left, right;
};
*)

(*
The F# standard library defines the following parametric type:

type 'a option = None | Some of 'a

It represents nullable value, i.e. values that can either be there or not be there.
There are 2 cases: None means there is no value; Some means there is something.

The following functions picks an int and an int option and produces an int.
The second argument (m) is therefore not an int: it is an int option, so it can either be None or Some attached to an int.
Pattern matching is needed to inspect such a value, as 2 possibile cases must be supported.

let optional_add n m = // f : int -> int option -> int
    match m with   
    | None -> n         // if m is nothing then just return n
    | Some x -> n + x   // otherwise it is Some x, with x : int which is bound to the value attached to the Some constructor

let caller () =
    let a = optional_add 5 None      // a : int = 5
    let b = optional_add 7 (Some 3)  // b : int = 8

Java is a language where nullness is a property of reference types only (i.e. objects, not builtin values such as integers, doubles etc.).
There is no optional type in Java: you simply use objects, which can be null, to represent optional values.
In Java the same thing would be:

public static int optionalAdd(int n, Integer m) 
    if (m != null) return n + m;
    else return n;
}
*)

// this is the type definition of a binary tree
// only 1 constructor is defined, so this is a heavyweight type that is actually isomorphic to a triple ('a * 'a tree option * 'a tree option)
// heavyweight types are real NEW types, such as union types (in F# also record types and class types exist)
// lightweight types are type aliases, i.e. a short name for an already existing type (like a typedef in C)
type 'a tree = Tree of 'a * 'a tree option * 'a tree option

let t1 = Tree (3, Some (Tree (5, None, None)), None)


// generate an int tree
let rnd = new System.Random ()
let rec generate_tree () =  // generate_tree : unit -> int tree
    let data = rnd.Next (0, 10)
    let branch () =
        if rnd.Next (0, 10) <= 2 then Some (generate_tree ())
        else None
    Tree (data, branch (), branch ())
    
// flatten a tree into a list
let rec flatten_tree t =    // flatten_tree : 'a tree -> 'a list
    match t with
    | Tree (data, None, None) -> [data]
    | Tree (data, Some b, None) 
    | Tree (data, None, Some b) -> data :: flatten_tree b
    | Tree (data, Some l, Some r) -> data :: flatten_tree l @ flatten_tree r


// fold a tree by folding its flattened list: this is the easy version
let fold_tree f z t = fold_left f z (flatten_tree t)

// natively for a tree: this is more complicated as the accumulator z must be properly updated and propagated across branches
let rec fold_tree' f z t =
    match t with
    | Tree (data, None, None) -> f z data
    | Tree (data, Some b, None)
    | Tree (data, None, Some b) -> fold_tree' f (f z data) b
    | Tree (data, Some l, Some r) -> 
        let z1 = fold_tree' f (f z data) l
        fold_tree' f (f z1 data) r


let test_tree () =
    let t1 = Tree (1, Some (Tree (2, None, None)), None)    // sample tree written by hand
    let t2 = generate_tree ()
    printf "%O ===> %A" t2 (flatten_tree t2)




