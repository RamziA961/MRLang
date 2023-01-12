module Transpiler.AbstractSyntaxTree.FuncTree

open Transpiler.Lexer.Token
open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.IdentifierTree
open Transpiler.AbstractSyntaxTree.DeclTree
open Transpiler.AbstractSyntaxTree.BlockTree

/// <summary>
/// A function that attempts to create a Function Declaration AST.
/// Function Declaration ASTs consist of a Type AST, Identifier AST, Arg Tree, and a Function body tree.
/// </summary>
/// <param name="tokens">A token list.</param>
/// <returns>Unparsed token list and a Function AST or None.</returns>
let rec Func (tokens: Token list) : Token list * AST option =
    match tokens with
    | PROC :: IDENTIFIER _ :: _ -> FuncDecl tokens
    | _ -> tokens, None // propagate None upwards
and FuncDecl (tokens: Token list) : Token list * AST option =
    match Identifier tokens[1..] with
    | ASSIGN :: tail, Some id ->
        let rem, args = FuncArgs tail
        let rem, retType = TypeDecl rem
        
        match rem, args, retType with
        | COLON :: rem , Some args, Some retType ->
            match FuncBody rem with
            | rem, Some body  -> rem, Some {
                  token = tokens[0]
                  decoration = "FuncTree"
                  children = [ retType; id; args; body ]
                }
            | _ -> tokens, None // propagate None upwards
        | _ -> tokens, None // propagate None upwards
    | _ -> tokens, None // propagate None upwards

and FuncArgs (tokens: Token list) : Token list * AST option =
    let rec Accumulate (tokens: Token list) (accumulator: AST list) : Token list * AST list option =
        match tokens with
        | Declaration _ ->
            match Decl tokens with 
            | SEP :: tail, Some ast -> Accumulate tail (accumulator @ [ast])
            | R_PAR :: _ as rem, Some ast -> Accumulate rem (accumulator @ [ast])
            | _ -> tokens, None
        | R_PAR :: tail -> tail, Some accumulator
        | _ -> tokens, None // propagate None upwards

    match tokens with
    | L_PAR :: tail ->
        match Accumulate tail [] with
        | rem, Some accum -> rem, Some {
                token = ARGS
                decoration = "ArgTree"
                children = accum
            }
        | _ -> tokens, None // propagate None upwards
    | _ -> tokens, None // propagate None upwards
    
/// <summary>
/// A function that attempts to create a Block AST.
/// </summary>
/// <param name="tokens">A token list.</param>
/// <returns>Unparsed tokens and a Block AST or None.</returns>
and FuncBody (tokens: Token list) : Token list * AST option =
    match tokens with
    | LINE_END :: tail -> FuncBody tail
    | BEGIN :: tail ->
        match Block tail with
        | END :: tail, Some ast -> tail, Some ast
        | _ -> tokens, None // propagate None upwards
    | _ -> tokens, None // propagate None upwards
