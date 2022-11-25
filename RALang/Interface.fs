module Transpiler.Interface

open Transpiler.Lexer
open Transpiler.Parser
open Transpiler.Writer

let Transpile (sourceCode : string) : string =
    let out = Lexer.Lex sourceCode |> Parser.Parse |> Write.GenerateSourceCode
    out