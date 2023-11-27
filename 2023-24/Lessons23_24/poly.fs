module poly


let test1 () =
    ((fun x -> x) 3, (fun x -> x) true)

let test2 () =
    let id = fun x -> x
    (id 3, id true)
