module Transpiler.AbstractSyntaxTree.DeclTree

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.IdentifierTree
open Transpiler.Lexer.Token


(*
    BNF
    <type> ::= 'INT' | 'REAL' | 'STRING' | 'BOOL'
    <identifier> ::= <char>{"_" | <identifier>}
    <decl> ::= <type> <identifier>';'
*)

/// <summary>
/// Active pattern for declaration matching.
/// </summary>
/// <param name="tokens">A token list.</param>
/// <returns>Some if first elements of list are a valid declaration, otherwise None.</returns>
/// <remarks>Does not match assignment operations.</remarks>
let (|Declaration|_|) (tokens: Token list) =
    match tokens with
    | TYPE _ :: IDENTIFIER _ :: (ASSIGN | MUTATE) :: _ -> None
    | TYPE _ :: IDENTIFIER _ :: _ -> Some(tokens)
    | _ -> None    
       
let isDecl (tokens : Token list) : bool =
    let rec Decl(tokens: Token list) = (TypeDecl >> IdentifierDecl) tokens
    and TypeDecl(tokens: Token list) =
        match tokens with
        | TYPE _ :: tail -> (tail, true)
        | _ :: tail -> (tail, false)
        | _ -> ([], false)
    and IdentifierDecl (tokens: Token list, value: bool) =
        match tokens with
        | IDENTIFIER _ :: tail -> (tail, value)
        | _ :: tail -> (tail, false)
        | _ -> ([], false)
    
    if tokens.Length = 2 then
        snd(Decl tokens)
    else
        false
     

/// <summary>
/// Attempts to create a Type AST from the head of a token list.
/// </summary>
/// <param name="tokens">A token list.</param>
/// <returns>Unparsed token list and Type AST or None.</returns>
let TypeDecl(tokens: Token list) : Token list * AST option =
    match tokens with
    | TYPE t :: tail -> (tail, Some {
            decoration = "TypeTree"
            token = TYPE t
            children = []
        })
    | _ -> tokens, None // propagate None upwards

/// <summary>
/// Attempts to create an Identifier AST from the Token head.
/// </summary>
/// <param name="tokens">A token list.</param>
/// <returns>Unparsed token list and an Identifier AST or None.</returns>
let private IdentifierDecl (tokens: Token list) : Token list * AST option =
    match tokens with
    | IDENTIFIER _ :: _ -> Identifier tokens
    | _ -> tokens, None // propagate None upwards

/// <summary>
/// Attempts to create a Declaration AST composed of a Type AST and Identifier AST.
/// </summary>
/// <param name="tokens"> A token list.</param>
/// <returns>Unparsed token list and a Declaration AST or None.</returns>
let Decl (tokens : Token list) : Token list * AST option =
    let tyRem, tyAST = TypeDecl tokens // initialize type ast
    let idRem, idAST = IdentifierDecl tyRem // initialize identifier ast
    
    match tyAST, idAST with
    | Some tyAST, Some idAST -> idRem, Some {
            decoration = "DeclTree"
            token = DECLARE
            children = [tyAST;  idAST]
        }
    | _ -> tokens, None // propagate None upwards