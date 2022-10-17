namespace Lexer

open System
open System.Text.RegularExpressions

module Lexer =
    
    
    let Filter(input: string): string =
        //Source of Regular Expression:
        //Finding Comments in Source Code Using Regular Expressions by Stephen Ostermiller
        //https://blog.ostermiller.org/finding-comments-in-source-code-using-regular-expressions/
        Regex.Replace(input, "/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/", "")    
        
    let Tokenize (input : string): string[] =
        input.Split [|' '; '\n'|]
        
    
    let Lex(input: string) : string[] =
        Filter input
        |> Tokenize