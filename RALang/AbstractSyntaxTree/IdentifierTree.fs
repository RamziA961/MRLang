module Transpiler.AbstractSyntaxTree.IdentifierTree

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.Lexer.Token

let Identifier (tokens: Token list) : Token list * AST =
    match tokens with
    | IDENTIFIER _ :: tail -> (tail, {
        decoration = "IdentifierTree"
        token = tokens.Head
        children = []
    })
    | _ -> raise( UnexpectedToken $"Unexpected token encountered: {tokens[0]}. Expected IDENTIFIER." )