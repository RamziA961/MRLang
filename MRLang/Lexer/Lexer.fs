namespace Lexer

open MRLang.Token
open System.Text.RegularExpressions


module Lexer =
    
    let private Filter input: string =
        //Source of Regular Expression:
        //Finding Comments in Source Code Using Regular Expressions by Stephen Ostermiller
        //https://blog.ostermiller.org/finding-comments-in-source-code-using-regular-expressions/
        Regex.Replace(input, "/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/", "")    
            
    let private Tokenize(input : string): List<string> =
        
        let rec Scan (input: string) (accumulator: List<string>) (cursor: int) (canSplit: bool) : List<string> =
            printfn $"%A{Token.Operator}"
            printfn $"%A{Token.Operator.Contains('+')}"

            if cursor >= input.Length then
                if input = "" then accumulator else (accumulator @ [input])
            else
                let SliceAndScan = Scan (input.Substring(cursor + 1))
                let Advance = Scan input accumulator (cursor + 1)

                match input[cursor] with
                | '"' when canSplit -> Advance false
                | '"' when not(canSplit) -> Advance true
                | (' ' | '\n') when (input[0 .. cursor - 1] <> "" && canSplit) -> 
                    SliceAndScan (accumulator @ [input[0 .. cursor - 1]]) 0 canSplit
                | (' ' | '\n') when canSplit ->
                    SliceAndScan accumulator 0 canSplit
                // | Token.Operator.Contains(input[cursor]) -> 
                    // SliceAndScan (accumulator @ [input[0 .. cursor -1]]) 0 canSplit
                | _ when Token.Operator.Contains(input[cursor]) ->
                    SliceAndScan (accumulator @ [input[0 .. cursor]]) 0 canSplit
                | _ -> Advance canSplit


        Scan input List.empty 0 true
        
    let Lex(input: string) : List<string> =
        input.Trim [|' '|]
        |> Filter
        |> Tokenize