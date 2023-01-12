module Transpiler.AbstractSyntaxTree.IdentifierTree

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.Lexer.Token

/// <summary>
/// A function that attempts to generate an identifier AST.
/// If the token list's head is not an identifier token, the function will return None.
/// </summary>
/// <param name="tokens">A token list.</param>
/// <returns>Unparsed token list and an identifier AST or None.</returns>
let Identifier (tokens: Token list) : Token list * AST option =
    match tokens with
    | IDENTIFIER _ :: tail -> tail, Some {
            decoration = "IdentifierTree"
            token = tokens.Head
            children = []
        }
    | _ -> tokens, None