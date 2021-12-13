module TinyML.Main

open FSharp.Text
open System
open TinyML.Parsing
open TinyML.Ast

exception SyntaxError of string * Lexing.LexBuffer<char>

let private __syntax_error (msg, lexbuf : Lexing.LexBuffer<char>) = SyntaxError (msg, lexbuf) //new syntax_error (msg, lexbuf)
let parse_from_TextReader args = parse_from_TextReader __syntax_error args

let load_and_parse_program filename =
   use fstr = new IO.FileStream (filename, IO.FileMode.Open)
   use rd = new IO.StreamReader (fstr)
   printfn "parsing source file '%s'..." filename
   parse_from_TextReader rd filename (1, 1) Parser.program Lexer.tokenize Parser.tokenTagToTokenId

let parse_from_string what p s = parse_from_string __syntax_error s (sprintf "%s:\"%s\"" what s) (0, 0) p Lexer.tokenize Parser.tokenTagToTokenId


[<EntryPoint>]
let main argv =
    let r =
        try
            let prg = load_and_parse_program argv.[0]
            printfn "parsed program:\n%s\n\nAST:\n%A" (pretty_expr prg) prg
            let t = Typing.typecheck_expr [] prg
            printfn "type: %s" (pretty_ty t)
            0
        with SyntaxError (msg, lexbuf) -> printfn "\nsyntax error: %s\nat token: %A\nlocation: %O" msg lexbuf.Lexeme lexbuf.EndPos; 1
           | TypeError msg             -> printfn "\ntype error: %s" msg; 2
           | UnexpectedError msg       -> printfn "\nunexpected error: %s" msg; 3
           | e                         -> printfn "\nexception caught: %O" e; 4
    Console.ReadLine () |> ignore
    r

