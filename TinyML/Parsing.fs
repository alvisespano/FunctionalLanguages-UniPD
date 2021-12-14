(*
* F# Common Library
* Parsing.fs: parsing facilities
* (C) 2007-2021 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module FSharp.Common.Parsing

open System
open System.Text.RegularExpressions
open FSharp.Text  // if removed, any project using types such as Position and LexBuffer will not compile because of name clashing with homonimous types defined elsewhere within F# libs

let parse_float s = Double.Parse (s, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture)

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



module Scanf =
   open Microsoft.FSharp.Reflection

   let check f x =
       if f x then x
       else failwithf "scanf format failure \"%s\"" x

   let parseDecimal x = Decimal.Parse (x, System.Globalization.CultureInfo.InvariantCulture)

   let parsers =
       dict [
           'b', Boolean.Parse >> box
           'd', int >> box
           'i', int >> box
           's', box
           'u', uint32 >> int >> box
           'x', check (String.forall Char.IsLower) >> ((+) "0x") >> int >> box
           'X', check (String.forall Char.IsUpper) >> ((+) "0x") >> int >> box
           'o', ((+) "0o") >> int >> box
           'e', float >> box // no check for correct format for floats
           'E', float >> box
           'f', float >> box
           'F', float >> box
           'g', float >> box
           'G', float >> box
           'M', parseDecimal >> box
           'c', char >> box
       ]

   // array of all possible formatters, i.e. [|"%b"; "%d"; ...|]
   let separators =
      parsers.Keys
      |> Seq.map (fun c -> sprintf "%%%c" c)
      |> Seq.toArray

   // Creates a list of formatter characters from a format string,
   // for example "(%s,%d)" -> ['s', 'd']
   let rec get_formatters = function
      | '%' :: '%' :: xr   -> get_formatters xr
      | '%' :: x :: xr     -> if parsers.ContainsKey x then x :: get_formatters xr else failwithf "unknown formatter %%%c" x
      | x :: xr            -> get_formatters xr
      | []                 -> []

   let sscanf (pf : PrintfFormat<_, _, _, _, 't>) s : 't =
       let fmts = pf.Value.Replace ("%%", "%")
       let constants = fmts.Split (separators, StringSplitOptions.None)
       let rx = new Regex ("^" + String.Join ("(.*?)", constants |> Array.map Regex.Escape) + "$")
       let formatters = pf.Value.ToCharArray () |> Array.toList |> get_formatters 
       let gs = rx.Match(s).Groups |> Seq.cast<Group> |> Seq.skip 1
       let ms = (gs, formatters) ||> Seq.map2 (fun g f -> g.Value |> parsers.[f]) |> Seq.toArray
       in
           if ms.Length = 1 then ms.[0] :?> 't
           else FSharpValue.MakeTuple (ms, typeof<'t>) :?> 't

let sscanf fmt s = Scanf.sscanf fmt s

