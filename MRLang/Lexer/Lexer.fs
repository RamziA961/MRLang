namespace Lexer

open MRLang.Token
open System.Text.RegularExpressions

module Lexer =
    
    let private Filter input: string =
        //Source of Regular Expression:
        //Finding Comments in Source Code Using Regular Expressions by Stephen Ostermiller
        //https://blog.ostermiller.org/finding-comments-in-source-code-using-regular-expressions/
        Regex.Replace(input, "/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/", "")    
            
    let private Tokenize (input : string) : string list  =
        
        let rec Scan (input: string) (accumulator: string list) (cursor: int) (canSplit: bool): string list =
            
            if cursor >= input.Length then
                if input = "" then accumulator else (accumulator @ [input])
            else
                let SliceAndScan = Scan (input.Substring(cursor + 1))
                let Advance = Scan input accumulator (cursor + 1)
                
                // might be insufficient for some cases such as "==" and "="
                match input[cursor] with
                | '"' when canSplit ->
                    // prevent splitting when first double quote found
                    Advance false
                    
                | '"' when not(canSplit) ->
                    // allow splitting when second double quote found
                    Advance true
                    
                | (' ' | '\n') when (input[0 .. cursor - 1] <> "" && canSplit) ->
                    // (space or newline character found) & (buffer not empty) & (not reading a string value)
                    SliceAndScan (accumulator @ [input[0 .. cursor - 1]]) 0 canSplit
                    
                | ' ' when canSplit ->
                    // filter out spaces between tokens
                    SliceAndScan accumulator 0 canSplit
                | '\n' ->
                    // preserve new line character to distinguish between lines
                    SliceAndScan (accumulator @ [input[0.. cursor]]) 0 canSplit
                    
                | _ when Token.BinaryOperator.Contains($"{input[cursor]}")
                         || Token.Parenthesis.Contains($"{input[cursor]}") ->
                    // binary operator or parenthesis found, extract token
                 
                    match input[cursor] with
                    | _ when cursor >= 1 ->
                        SliceAndScan (accumulator @ [input[0 .. cursor-1 ]] @ [input[cursor..cursor]]) 0 canSplit
                    | _ -> SliceAndScan (accumulator @ [input[0 .. cursor]]) 0 canSplit
                    
                | _ ->
                    Advance canSplit


        Scan input [] 0 true
        
    let Lex (input : string) : string list =
        input.Trim [|' '|]
        |> Filter
        |> Tokenize