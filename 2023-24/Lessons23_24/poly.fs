module poly


let test1 () =
    ((fun x -> x) 3, (fun x -> x) true)

let test2 () =
    let id = fun x -> x
    (id 3, id true)


let a =
    let f = (fun x -> (let x = x in x)) in f    
        
let f = fun (x : 'a) -> fun (y : 'c -> 'c -> 'd) -> fun (z : 'c) -> 
    let x = y
    let y = let x = x in z
    x y z

let g =
    let a = 8
    fun x -> a 

let g' = fun x ->
    let a = 8
    a 

let h = fun x -> fun z ->
    let f = fun y -> y + z
    f 3 + x

let rec map f =
    let R l = map f l
    fun l ->
        match l with
        | [] -> []
        | h :: t -> f h :: R t


let rec list_length l =
    match l with
    | [] -> 0
    | _ :: t -> 1 + list_length t






