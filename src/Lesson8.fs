module Lesson8

open Lesson7

(*
struct tree_node {
    int data;
    struct tree_node* left, right;
};
*)

//type 'a option = None | Some of 'a

let f n m = 
    match m with
    | None -> n
    | Some x -> n + x

(*
public static int f(int n, Integer m) 
    if (m != null) return n + m;
    else return n;
}

*)


type 'a tree = Tree of 'a * 'a tree option * 'a tree option

let rnd = new System.Random ()
let rec generate_tree () =
    let data = rnd.Next (0, 10)
    let branch () =
        if rnd.Next (0, 10) <= 2 then Some (generate_tree ())
        else None
    Tree (data, branch (), branch ())
    

let rec flatten_tree t =
    match t with
    | Tree (data, None, None) -> [data]
    | Tree (data, Some b, None) 
    | Tree (data, None, Some b) -> data :: flatten_tree b
    | Tree (data, Some l, Some r) -> data :: flatten_tree l @ flatten_tree r

let fold_tree f z t = fold_left f z (flatten_tree t)

let rec fold_tree' f z t =
    match t with
    | Tree (data, None, None) -> f z data
    | Tree (data, Some b, None)
    | Tree (data, None, Some b) -> fold_tree' f (f z data) b
    | Tree (data, Some l, Some r) -> 
        let z1 = fold_tree' f (f z data) l
        fold_tree' f (f z1 data) r


let test_tree () =
    let t1 = Tree (1, Some (Tree (2, None, None)), None)
    let t2 = generate_tree ()
    printf "%O ===> %A" t2 (flatten_tree t2)
