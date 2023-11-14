module trees

type 'a bintree = Leaf | Node of 'a * 'a bintree * 'a bintree

let tree1 = Node (1, Node (2, Node (3, Leaf, Leaf), Leaf), Node (5, Leaf, Leaf))

let rec print_bintree t =
    match t with
    | Leaf -> ()
    | Node (data, left, right) ->
        printf "%O\n" data;
        print_bintree left;
        print_bintree right