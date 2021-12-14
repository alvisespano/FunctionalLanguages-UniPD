(*
* TinyML
* Main.fs: main code
*)

module TinyML.Main

open System
open FSharp.Common
open TinyML.Ast

let parse_from_TextReader rd filename parser = Parsing.parse_from_TextReader SyntaxError rd filename (1, 1) parser Lexer.tokenize Parser.tokenTagToTokenId

let load_and_parse_program filename =
   use fstr = new IO.FileStream (filename, IO.FileMode.Open)
   use rd = new IO.StreamReader (fstr)
   printfn "parsing source file '%s'..." filename
   parse_from_TextReader rd filename Parser.program 

let interpret_expr e =
    printfn "AST:\t%A\npretty:\t%s" e (pretty_expr e)
    let t = Typing.typecheck_expr [] e
    printfn "type:\t%s" (pretty_ty t)
    let v = Eval.eval_expr [] e
    printfn "value:\t%s" (pretty_value v)


let main_interpreter filename =
    try
        let prg = load_and_parse_program filename
        interpret_expr prg
    with SyntaxError (msg, lexbuf) -> printfn "\nsyntax error: %s\nat token: %A\nlocation: %O" msg lexbuf.Lexeme lexbuf.EndPos
        | TypeError msg             -> printfn "\ntype error: %s" msg
        | UnexpectedError msg       -> printfn "\nunexpected error: %s" msg

let main_interactive () =
    while true do
        try
            printf "\n> "
            stdout.Flush ()
            let prg = parse_from_TextReader stdin "LINE" Parser.interactive
            interpret_expr prg
        with SyntaxError (msg, lexbuf) -> printfn "\nsyntax error: %s\nat token: %A\nlocation: %O" msg lexbuf.Lexeme lexbuf.EndPos
           | TypeError msg             -> printfn "\ntype error: %s" msg
           | UnexpectedError msg       -> printfn "\nunexpected error: %s" msg
    
[<EntryPoint>]
let main argv =
    let r =
        try 
            if argv.Length < 1 then main_interactive ()
            else main_interpreter argv.[0]
            0
        with e -> printfn "\nexception caught: %O" e; 1
    Console.ReadLine () |> ignore
    r
