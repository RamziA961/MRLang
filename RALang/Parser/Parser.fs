module Transpiler.Parser.Parser

open Transpiler.Lexer.Token

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.BlockTree
open Transpiler.AbstractSyntaxTree.FuncTree


let Parse (tokens: Token list) : AST =
    let SymbolMap = Set.empty
    
    let rec ConstructProgramTree (tokens: Token list) (ast: AST) : Token list * AST =
        match tokens with
        | LINE_END :: tail -> ConstructProgramTree tail ast
        | PROC :: _ ->
            let rem, funcTree = Func tokens
            ConstructProgramTree rem { ast with children = ast.children @ [ funcTree ] }
        | [] -> tokens, ast
    
    snd (ConstructProgramTree tokens { token = PROGRAM; decoration = "ProgramTree"; children = [] })