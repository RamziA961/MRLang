module Transpiler.AbstractSyntaxTree.StatementTree

open Transpiler.Lexer.Token
open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.AssignTree
open Transpiler.AbstractSyntaxTree.DeclTree

let isStatement (accumulated: Token list) = isAssign accumulated || isDecl accumulated

let Statement (accumulated: Token list) =
    match accumulated with
    | _ when isDecl accumulated -> Decl accumulated 
    | _ when isAssign accumulated -> Assign accumulated
    | _ -> raise (UnexpectedToken $"Unexpected tokens encountered: %A{accumulated}")