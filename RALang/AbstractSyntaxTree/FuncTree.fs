module Transpiler.AbstractSyntaxTree.FuncTree

open Transpiler.Lexer.Token
open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.IdentifierTree
open Transpiler.AbstractSyntaxTree.DeclTree
open Transpiler.AbstractSyntaxTree.BlockTree


let rec Func (tokens: Token list) : Token list * AST =
    match tokens with
    | PROC :: IDENTIFIER _ :: _ -> FuncDecl tokens
    | _ -> raise (UnexpectedToken $"Unexpected token detected. Expected: PROC IDENTIFIER.\nFound: {tokens[0..2]}")

and FuncDecl (tokens: Token list) : Token list * AST =
    match tokens[1..] with
    | IDENTIFIER _ :: ASSIGN :: _ ->
        let rem, id = Identifier tokens[1..]
        match rem with
        | ASSIGN :: tail ->
            let rem, args = FuncArgs tail
            let rem, retType = TypeDecl rem
            
            match rem with
            | COLON :: tail ->
                let rem, body = FuncBody tail
                rem, {
                  token = tokens[0]
                  decoration = "FuncTree"
                  children = [ retType; id; args; body ]
                }
            | _ -> raise (UnexpectedToken $"Unexpected token detected. Expected: COLON.\nFound: {tokens[0]}")
        | _ -> raise (UnexpectedToken $"Unexpected token detected. Expected: ASSIGN.\nFound: {tokens[0]}")
    | _ -> raise (UnexpectedToken $"Unexpected token detected. Expected: IDENTIFIER.\nFound: {tokens[0]}")

and FuncArgs (tokens: Token list) : Token list * AST =
    let rec Accumulate (tokens: Token list) (accumulator: AST list) : Token list * AST list =
        match tokens with
        | TYPE _ :: IDENTIFIER _ :: _ ->
            let rem, ast = Decl tokens
            match rem with
            | SEP :: tail -> Accumulate tail (accumulator @ [ ast ])
            | R_PAR :: _ -> Accumulate rem (accumulator @ [ ast ])
            | _ -> raise (UnexpectedToken $"Unexpected token detected. Expected: SEP or R_PAR.\nFound: {tokens[0]}")
        | R_PAR :: tail -> tail, accumulator
        | _ -> raise (UnexpectedToken $"Unexpected token detected. Expected: TYPE or R_PAR.\nFound: {tokens[0]}")

    match tokens with
    | L_PAR :: tail ->
        let rem, accum = Accumulate tail []
        rem, {
            token = ARGS
            decoration = "ArgumentTree"
            children = accum
        }
    | _ -> raise (UnexpectedToken $"Unexpected token detected. Expected: L_PAR.\nFound: {tokens[0]}")

and FuncBody (tokens: Token list) : Token list * AST =
    match tokens with
    | LINE_END :: tail -> FuncBody tail
    | BEGIN :: tail ->
        let rem, ast = Block tail
        match rem with
        | END :: tail -> tail, ast
        | _ -> raise (UnexpectedToken $"Unexpected token detected. Expected: END.\nFound: {rem[0]}")
    | _ -> raise (UnexpectedToken $"Unexpected token detected. Expected: BEGIN.\nFound: {tokens[0]}")
