module Transpiler.Lexer.Lexer

open System.Text.RegularExpressions
open Token


exception UnsupportedKeywordError of string
    
let private Filter input: string =
    //Source of Regular Expression:
    //Finding Comments in Source Code Using Regular Expressions by Stephen Ostermiller
    //https://blog.ostermiller.org/finding-comments-in-source-code-using-regular-expressions/
    Regex.Replace(input, "/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/", "")    
            
        
let private Tokenize (inputString : string) : Token list =
    let MatchToken (input : string) (accumulated : Token list): Token =
        match input with
        | "T" | "F" ->
            B (input = "T")
        | _ when input.StartsWith "\"" && input.EndsWith "\"" ->
            S input
        | _ when BOpTokenMap.ContainsKey input -> 
            BOpTokenMap.Item input
        | _ when ROpTokenMap.ContainsKey input ->
            ROpTokenMap.Item input
        | _ when IsFloat input ->
            F (float input)
        | _ when IsInteger input ->
            I (int input)
        | "(" | ")" ->
            ParenthesisTokenMap.Item input
        | "\n" | ";" ->
            LINE_END
        | _ when Types.Contains input ->
            TYPE input
        | _ when isIdentifier input ->
            IDENTIFIER input
        | "=" -> ASSIGN
        | ":=" -> MUTATE
        | _ ->
            raise( UnsupportedKeywordError $"Lexing Error: '%s{input}' is not a supported keyword." )
                
    let rec Scan (input: string) (accumulator : Token list) (cursor : int) (canSplit : bool) : Token list =
        if cursor >= input.Length then
            if input = "" then accumulator @ [LINE_END] else accumulator @ [MatchToken input accumulator; LINE_END]
        else
            let SliceAndScan = Scan (input.Substring(cursor + 1))
            let Advance = Scan input accumulator (cursor + 1)
               
            match input[cursor] with
            | '"' when canSplit ->
                // prevent splitting when first double quote found
                Advance false
                    
            | '"' when not canSplit ->
                // allow splitting when second double quote found
                Advance true
            | ' ' when input[0 .. cursor - 1] <> "" && canSplit ->
                // (space or newline character found) & (buffer not empty) & (not reading a string value)
                SliceAndScan (accumulator @ [MatchToken(input[0 .. cursor - 1]) accumulator]) 0 canSplit
            | '\n' | ';' when input[0 .. cursor - 1] <> "" && canSplit ->
                SliceAndScan (
                    accumulator @
                    [MatchToken(input[0 .. cursor - 1]) accumulator] @
                    [MatchToken($"{input[cursor]}") accumulator]
                ) 0 canSplit
            |' ' | '\n' | ';' when canSplit ->
                Scan input[1..] accumulator cursor canSplit
            | ' ' when canSplit ->  
                // filter out spaces between tokens
                SliceAndScan accumulator 0 canSplit
            | '-' when canSplit ->
                if cursor >= 1 then
                    Scan (input.Substring cursor) (accumulator @ [MatchToken input[0.. cursor-1] accumulator]) 1 canSplit 
                else
                    Advance canSplit
            | _ when BOpTokenMap.ContainsKey($"{input[cursor]}")
                || ROpTokenMap.ContainsKey($"{input[cursor]}")
                || ParenthesisTokenMap.ContainsKey($"{input[cursor]}") ->
                if cursor >= 1 then
                    SliceAndScan
                        (accumulator @
                            [MatchToken(input[0 .. cursor-1]) accumulator; MatchToken(input[cursor..cursor]) accumulator]
                        )
                        0 canSplit
                else
                    SliceAndScan (accumulator @ [MatchToken(input[0 .. cursor]) accumulator]) 0 canSplit
            | _ ->
                Advance canSplit
    Scan inputString [] 0 true              
      
let Lex (input : string) : Token list =
    input.Trim [|' '|]
    |> Filter
    |> Tokenize