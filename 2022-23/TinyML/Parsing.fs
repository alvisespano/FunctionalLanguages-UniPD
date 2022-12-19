(*
* F# Common Library
* Parsing.fs: parsing facilities
* (C) 2007-2021 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module FSharp.Common.Parsing

#nowarn "52"

open System
open FSharp.Text  // if removed, any project using types such as Position and LexBuffer will not compile because of name clashing with homonimous types defined elsewhere within F# libs

let parse_float (s : string) = Double.Parse (s, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture)

// list of strings mappers
//

let mappen_strings_or_nothing f empty sep xs =
    match Seq.toList xs with
      | []      -> empty
      | [x]     -> f x
      | x :: xs -> Seq.fold (fun r x -> r + sep + (f x)) (f x) xs

let flatten_strings_or_nothing empty = mappen_strings_or_nothing id empty

let flatten_strings sep = flatten_strings_or_nothing "" sep
let mappen_strings f = mappen_strings_or_nothing f ""

let mappen_stringables f = mappen_strings (f >> sprintf "%O")
let flatten_stringables sep = mappen_stringables id sep


(* error locating *)

// yacc/lex utilities
//

module LexYacc =
   exception ParseErrorContextException of obj
   let parse_error_rich = Some (fun ctx -> raise (ParseErrorContextException ctx))
   let newline (lexbuf : Lexing.LexBuffer<_>) = lexbuf.EndPos <- lexbuf.EndPos.NextLine
   let lexeme = Lexing.LexBuffer<_>.LexemeString


// FsYacc parser wrapper
//

let yparse syntax_error parser (tokenizer : Lexing.LexBuffer<_> -> 'tok) tokenTagToTokenId =
   let pretty_token_by_tags =
       let p n = sprintf "%A" (tokenTagToTokenId n)  // do not change %A with %O: tokens do not define ToString(), thus only %A prints them
       let prefix = "TOKEN_"
       in
           fun ns ->
               let r = mappen_strings (fun n -> let s = p n in sprintf "<%s>" (if s.StartsWith prefix then s.Remove (0, prefix.Length) else s)) ", " ns
               in
                   if List.length ns > 1 then sprintf "one among %s" r
                   else r
   let tokenizer lexbuf =
       let tok =
           try tokenizer lexbuf
           with e -> raise (syntax_error (e.Message, lexbuf))
//        L.debug Low "%s" (pretty_token tok)
       tok
   in
       fun (lexbuf : Lexing.LexBuffer<_>) ->
           try parser tokenizer lexbuf
           with LexYacc.ParseErrorContextException ctx as e ->
                   let ctx = ctx :?> Parsing.ParseErrorContext<'tok>
                   let tok = match ctx.CurrentToken with Some t -> sprintf "<%A>" t | None -> "unknown"
                   let msg = sprintf "encountered token %s but expected %s%s"
                               tok
                               (pretty_token_by_tags ctx.ShiftTokens)
                               (let l = ctx.ReduceTokens in if l.IsEmpty then "" else sprintf ", or alternatively %s" (pretty_token_by_tags l))
                   in
                       raise (syntax_error (msg, lexbuf))

let init_lexbuf filename (start_line, start_col) (lexbuf : Lexing.LexBuffer<_>) =
   let r = { Lexing.Position.pos_bol = 0
             Lexing.Position.pos_fname = filename
             Lexing.Position.pos_cnum = start_col
             Lexing.Position.pos_lnum = start_line }
   lexbuf.StartPos <- r
   lexbuf.EndPos <- r

let parse_from_lexbuf syntax_error lexbuf filename (start_line, start_col) parser tokenizer tokenTagToTokenId =
   init_lexbuf filename (start_line, start_col) lexbuf
   yparse syntax_error parser tokenizer tokenTagToTokenId lexbuf

let parse_from_TextReader syntax_error trd = parse_from_lexbuf syntax_error (Lexing.LexBuffer<_>.FromTextReader trd)

let parse_from_string syntax_error s name = parse_from_lexbuf syntax_error (Lexing.LexBuffer<_>.FromString s) (sprintf "<%s>" name)
