namespace Transpiler.AbstractSyntaxTree.AbstractSyntaxTree

open Transpiler.Lexer.Token


exception TokenMatchingError of string
exception SyntaxError of string
exception InvalidOperation of string
exception UnexpectedToken of string

/// Abstract syntax tree type. Instances of this type are created during parsing and all nodes in the
/// program abstract syntax tree are of this type.
type AST = {
    children : AST list
    decoration : string
    token : Token
}