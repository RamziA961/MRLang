module Transpiler.AbstractSyntaxTree.FuncCallTree

open Transpiler.Lexer.Token
open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.IdentifierTree
open Transpiler.AbstractSyntaxTree.ExprTree

(* BNF
    <arg> ::= <identifier> | <expr>
    <args> ::= {<arg>','}[arg]
    <function-call> ::= <identifier>'('<args>')'
*)

/// <summary>
///  Attempts to create an Function Call AST. It consists of an Identifier AST and an Args AST.
/// </summary>
/// <param name="tokens"></param>
/// <returns>Unparsed tokens and a Function Call AST or None.</returns>
let rec FuncCall (tokens: Token list): Token list * AST option =
    match tokens with
    | IDENTIFIER _ :: L_PAR :: _ ->
            match Identifier tokens, FuncArgs tokens[1..] with
            | (_, Some idBranch), (rem, Some argBranch) -> rem, Some {
                    token = PROC_CALL
                    decoration = "FuncCallTree"
                    children = [idBranch; argBranch]
                }
            | _ -> tokens, None // propagate None upwards
    | _ -> tokens, None // propagate None upwards
    
/// <summary>
/// Attempts to create an Args AST. It can consist of Identifiers and/or Expressions.
/// </summary>
/// <param name="tokens"> A token list.</param>
/// <returns>Unparsed tokens and a Args AST or None.</returns>
and FuncArgs (tokens: Token list): Token list * AST option =
    
    let rec Accumulate (tokens: Token list) (accumulator: AST list): Token list * AST list option =
        match tokens with
        | [] -> tokens, Some accumulator
        | IDENTIFIER _ :: SEP :: tail ->
            let id = snd (Identifier tokens)
            match id with
            | Some id -> Accumulate tail (accumulator @ [id])
            | None -> tokens, None
        | R_PAR :: tail -> tail, Some accumulator
        | Expr tokens  ->
            let rem, expr = Expr tokens
            match expr with
            | Some expr -> Accumulate rem (accumulator @ [expr])
            | None -> tokens, None
        | _ -> tokens, None 
    
    match tokens with
    | L_PAR :: tail ->
        let rem, accum = Accumulate tail []
        match accum with
        | Some accum -> rem, Some { token = ARGS; decoration = "ArgTree"; children = accum }
        | None -> tokens, None
    | _ -> tokens, None
    