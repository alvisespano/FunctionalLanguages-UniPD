
module FL2022_23.Lesson3


///////////////////////////////
// simple list values
///////////////////////////////

let l0 = [ (fun x -> x + 1); (fun x -> x * 2)]

let l1 = [1; 5; 9; 10 + 3; 89]

let l2 = [true; false; true && false || true]



///////////////////////////////
// lists and basic pattern matching
///////////////////////////////

// lists are defined with a disjoin union type with 2 cases: we commented it because they are already defined by the F# standard library
//type 'a list = []                     // empty list case: [] is just a name
//             | (::) of 'a * 'a list   // non-empty list case: operator (::) is INFIX and is attached to a pair: the first element of the pair is the head of the list, the second element is the tail


// computes the lenght of a list recursively
let rec length_of_list l =
    match l with
    | [] -> 0                               // [] represents the base case of the recursion
    | x :: xs -> 1 + length_of_list xs      // x and xs are free variable names: they are bound to the RIGHT and the LEFT arguments of (::), which are the head and the tail of the list


// lets redefine the list type without shadowing the original definition in the F# standard library
type 'a list = Empty                        // empty list case
             | Cons of 'a * 'a list     // non-empty list case: rather than an infix operator we use a normal name attacched to a paiur

// the function lenght_of_list above would become like this
let rec length_of_list2 l =
    match l with
    | Empty -> 0                                    // Empty represents the base case of the recursion
    | Cons (x, xs) -> 1 + length_of_list2 xs    // x and xs are bound to the 2 arguments of NonEmpty: x is the head and xs is the tail

// in C that would be like:
(*
struct list_node
{
    int data;
    struct list_node* next;
};

unsigned int size_of_list(struct list_node* n)
{
    if (n == NULL)
        return 0;
    return size_of_list(n->next) + 1;
}
*)


///////////////////////////////
// example of another disjoint union type
///////////////////////////////

// this means: we introduce a new type 'color' which consists of 4 possibile values: Red, Black, Yellow and Rgb.
// the last value, namely Rgb, is attacched to a triple of integers
type color = Red | Black | Yellow | Rgb of int * int * int

let col1 = Black    // col1 : color

let violet = Rgb (255, 0, 255)


// pretty print a color
let pretty_print_color c =  // pretty_print_color : color -> string
    match c with
    | Red -> "red"
    | Black -> "black"
    | Yellow -> "yellow"
    | Rgb (r, g, b) -> sprintf "[R:%d G:%d B%d]" r g b  // just a way for printing an RGB color

// str1 : string
let str1 = pretty_print_color Red


module Tree =

    type tree = Leaf of int | Node of tree * tree

    let rec sum_tree t =
        match t with
        | Leaf n -> n

        | Node (t1, t2) -> sum_tree t1 + sum_tree t2


    let tree1 = Leaf 56

    let tree2 = Node (tree1, tree1)

    let tree3 = Node (Node (Node (Leaf 1, Leaf 2), Leaf 3), Leaf 4)



module MyList =


    type 'a mylist = Empty | Cons of 'a * 'a mylist 

    let ll1 = Empty
    let ll2 = Cons ("ciao", Cons ("sono", Cons (3, Empty)))
    let ll3 = Cons (true, Cons (true, Cons (false, Empty)))






// in C the same would be an enumeration, but enumerations do not support data attached to labels
(*
enum color { RED, BLACK, YELLOW };  // only the labels without data attached to it can be represented in C

color col1 = BLACK; // col1 has type color, not int!

const char* pretty_print_color(color c)
{
    switch(c) {
    case RED: return "red";
    case YELLOW: return "yellow";
    case BLACK: return "black";
    // but we dont' have a way for matching the RGB case, because it has 3 integers attached to it and this is not possible in C
}
*)




