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
     

let private TypeDecl(tokens: Token list) : Token list * AST =
    match tokens with
    | TYPE t :: tail -> (tail, {
            decoration = "TypeTree"
            token = TYPE t
            children = []
        })
    | _ -> raise(UnexpectedToken $"Unexpected token encountered: {tokens[0]}. Expected TYPE.")
        
let private IdentifierDecl (tokens: Token list) : Token list * AST =
    match tokens with
    | IDENTIFIER _ :: _ ->
        let remTokens, id = Identifier tokens
        (remTokens, id)
    | _ -> raise(UnexpectedToken $"Unexpected token encountered: {tokens[0]}. Expected IDENTIFIER.")

let Decl (tokens : Token list) : Token list * AST =
    let tyRem, tyAST = TypeDecl tokens
    let idRem, idAST = IdentifierDecl tyRem
    
    (idRem, {
        decoration = "DeclTree"
        token = DECLARATION
        children = [tyAST;  idAST]
    })