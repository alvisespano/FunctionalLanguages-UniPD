module Lesson2


let k = 4

let f = fun x -> x + k

let g (x, y, z) = (x + 1, y + z)

let g2 = fun (x, y, z) -> (x + 1, y + z)

///////////////////////////////

let rec size_of_list l =
    match l with
    | [] -> 0
    | h :: t -> 1 + size_of_list t


type color = Red | Black | Yellow | Rgb of int * int * int

let col1 = Black

let pretty_print_color c =
    match c with
    | Red -> "red"
    | Black -> "black"
    | Yellow -> "yellow"
    | Rgb (r, g, b) -> sprintf "[R:%d G:%d B%d]" r g b

let str1 = pretty_print_color Red



type 'a list = [] | (::) of 'a * 'a list





















    (*
enum Color { RED, YELLOW, BLACK, WHITE, BLUE };

int n = 3;
Color col1 = RED;

switch(col1) {
case RED: ...
case YELLOW: ..
default: ...
}


switch(n) {
case 0: do something; break;
case 1: ...
default: ...
}

*)

(*struct list_node {
    int data;
    struct list_node* next;
};

unsigned int size_of_list(struct list_node* n) {
    if (n == NULL)
        return 0;
    return size_of_list(n->next) + 1;
}*)











