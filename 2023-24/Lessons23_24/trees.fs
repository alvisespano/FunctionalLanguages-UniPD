module trees

type 'a bintree = Leaf | Node of 'a * 'a bintree * 'a bintree

let tree1 = Node (1, Node (2, Node (3, Leaf, Leaf), Leaf), Node (5, Leaf, Leaf))

let rec ugly_print_bintree t =
    match t with
    | Leaf -> ()
    | Node (data, left, right) ->
        printf "%O\n" data;
        ugly_print_bintree left;
        ugly_print_bintree right

let rec pretty_print_bintree t =
    match t with
    | Leaf -> ()
    | Node (data, left, right) ->
        printf "%O(" data;
        pretty_print_bintree left;
        printf ")[";
        pretty_print_bintree right;
        printf "]"

let print_bintree t =
    let rec R t =
        match t with
        | Leaf -> ""

        | Node (data, Leaf, Leaf) -> 
            sprintf "%O" data

        | Node (data, left, Leaf) -> 
            sprintf "%O(%s)" data (R left)

        | Node (data, Leaf, right) -> 
            sprintf "%O[%s]" data (R right)

        | Node (data, left, right) ->
            sprintf "%O(%s)[%s]" data (R left) (R right)
    printf "%s\n" (R t)


// filter : ('a -> bool) -> 'a bintree -> 'a bintree
let rec filter p t =
    let R = filter p
    match t with
    | Leaf -> Leaf
    | Node (data, left, right) ->
        if p data then Node (data, R left, R right)
        else Leaf

// map : ('a -> 'b) -> 'a bintree -> 'b bintree
let rec map f t =
    match t with
    | Leaf -> Leaf
    | Node (data, left, right) -> Node (f data, map f left, map f right)

// iter : ('a -> unit) -> 'a bintree -> unit
let rec iter f t =
    match t with
    | Leaf -> ()
    | Node (data, left, right) -> f data; iter f left; iter f right
       
// sum : ('a -> 'b -> 'b) -> 'b -> 'a bintree -> 'b
let rec sum (+) zero t =
    match t with
    | Leaf -> zero
    | Node (data, left, right) -> data + sum (+) zero left + sum (+) zero right


// fold : ('a -> 'b -> 'b) -> 'b -> 'a bintree -> 'b
let rec fold f accu0 t =
    match t with
    | Leaf -> accu0
    | Node (data, left, right) ->
        //let accu1 = f data accu0
        //let accu2 = fold f accu1 left
        //fold f accu2 right
        fold f (fold f (f data accu0 left) right

// TODO fold_back over bintrees








        



