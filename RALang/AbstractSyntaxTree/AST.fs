namespace Transpiler.AbstractSyntaxTree.AbstractSyntaxTree

open Transpiler.Lexer.Token

type AST =
    {
        children : AST list
        decoration : string
        token : Token
    }

    member this.AppendChild (child: AST) : AST =
        { this with children = this.children @ [child] }