module Transpiler.AbstractSyntaxTree.IdentifierTree

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.Lexer.Token

// let isIdentifier (token: Token) : bool =
//

let Identifier (token: Token) : AST =
    { decoration = "IdentifierTree"; token = token; children = [] }