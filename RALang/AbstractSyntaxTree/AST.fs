namespace Transpiler.AbstractSyntaxTree.AbstractSyntaxTree

open Transpiler.Lexer.Token


exception TokenMatchingError of string
exception SyntaxError of string
exception InvalidOperation of string
exception UnexpectedToken of string


type AST = {
    children : AST list
    decoration : string
    token : Token
}