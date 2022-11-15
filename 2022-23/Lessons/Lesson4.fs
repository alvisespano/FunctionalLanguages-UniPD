module Lesson4

module Map =

    // map : ('a -> 'b) -> 'a list -> 'b list
    let rec map f l =
        match l with
        | [] -> []
        | x :: xs -> f x :: map f xs

    let bool_list = map (fun x -> x >= 10) [3; 88; 4; 10]

    let strings_to_ints = map (fun s -> String.length s) ["ciao"; "sono"; "alvise"]

    // add1 : int * int -> int
    let add_uncurried = fun (x, y) -> x + y

    // add2 : int -> int -> int
    let add_curried = fun x -> (fun y -> x + y)

    let call_uncurried = add_uncurried (3, 4)     // uncurried form, i.e. with tuples

    let call_curried = add_curried 3 4        // CURRIED form

    let f = add_curried 8

    let ten = f 2

    let map_length = map String.length

    let map_id x = map id x

    (*
    public static <A, B> List<B> map(Function<A, B> f, List<A> l) {
        List<B> r = new ArrayList<B>();
        for (A x : l) {
            B b = f.apply(x);
            r.add(b);
        }
        return r;
    }
    *)

module Filter =

    // filter : ('a -> bool) -> 'a list -> 'a list
    let rec filter f l =
        match l with
        | [] -> []
        | x :: xs -> if f x then x :: filter f xs else filter f xs

    let ex1 = filter (fun x -> x > 5) [1 .. 30]

    let ex2 = filter id (Map.map (fun x -> x < 10) [1 .. 20])


module Sum =
    
    let rec sum_ints l =
        match l with 
        | [] -> 0
        | x :: xs -> x + sum_ints xs

    let rec sum plus zero l =
        match l with 
        | [] -> zero
        | x :: xs -> plus x (sum plus zero xs)

    let ex1 = sum (+) 0. [1.0; 2.22; 67.34]

    let ex2 = sum (+) 0 [1 .. 20]

    let ex3 = sum (+) "" ["ciao"; "pippo"; "baudo"]

    let ex4 = sum (fun a b -> a || b) false [true; false; true; false]


module Iter =
    
    (*for (MyType x : e) {
        // do something
    }

    List<Integer> l = new ArrayList<>();
    for (Integer n : l) {
        System.out.println(n);
    }*)


    (*void f(int x) {
        x++;   
    }*)


    //type unit = ()

    // iter : ('a -> unit) -> 'a list -> unit
    let rec iter f l =
       match l with 
       | [] -> ()
       | x :: xs -> f x; iter f xs

    let () = iter (fun n -> printf "%d\n" n) [1 .. 20]

    // r : unit list
    let r = Map.map (fun n -> printf "%d\n" n) [1 .. 20]


module Fold =
    
    let rec foldR f z l =
        match l with
        | [] -> z
        | x :: xs -> f (foldR f z xs) x


    // foldL : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b
    let rec foldL f z l =
        match l with
        | [] -> z
        | x :: xs -> foldL f (f z x) xs

    // map : ('a -> 'b) -> 'a list -> 'b list
    let map f l = foldR (fun l2 x -> l2 @ [f x]) [] l

    // filter : ('a -> bool) -> 'a list -> 'a list
    let filter p l = foldL (fun z x -> if p x then x :: z else z) [] l


    // max_by : ('a -> 'a -> bool) -> 'a list -> 'a
    let max_by cmp l = 
        let rec aux m l =
            match l with
            | [] -> raise (Failure "message")
        aux ...

    let rec max_by cmp l =
        match l with
        | [] -> raise (Failure "message")
        | [x] -> x
        | x :: xs -> let m = max_by cmp xs in if cmp x m then m else x


    type 'a option = None | Some of 'a

    let max_by cmp l =
        let f m x =
            match m with
            | None -> Some x
            | Some y -> if cmp x y then Some y else Some x
        foldL f None l




    let r1 = foldL (fun z x -> x + z) 0 [1 .. 20] 

    let l2 = [1; 2; 3] @ [4; 5; 6]

    let s1 = foldL (+) "" ["a"; "b"; "c"]   // "abc"
    let s2 = foldR (+) "" ["a"; "b"; "c"]   // "cba"

    let factorial n = foldL ( * ) 1 [1 .. n]


    (*
    public static <A, B> List<B> map(Iterable<A> c, Function<A, B> f) {
        List<B> out = new ArrayList<>();
        for (A a : c)
            out.add(f.apply(a));
        return out;
    }

    template <class A, class B, class F>
    vector<B> map(const vector<A>& v, const F& f) {
        vector<B> r;
        for (auto x : v)
            r.push_back(f(x));
        return r;
    }
    *)



//// EXERCISE

module Exercise1 =

    type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree

    // define map, fold, sum, iter, filter etc for trees


















