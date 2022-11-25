module Transpiler.Program

open System.Text.Json

[<EntryPoint>]
let op args =
    let s = Lexer.Lexer.Lex "int x = 5;int y = 2;bool b = x == y;x := y;b := x == y;"
    
    let ast = Parser.Parser.Parse s
    
    printf $"{Writer.Write.GenerateSourceCode ast}"
    0