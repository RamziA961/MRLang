module Transpiler.AbstractSyntaxTree.FuncCallTree

open Transpiler.Lexer.Token
open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.IdentifierTree
open Transpiler.AbstractSyntaxTree.ExprTree


let rec FuncCall (tokens: Token list): Token list * AST =
    match tokens with
    | IDENTIFIER _ :: L_PAR :: _ -> FuncArgs tokens[1..]
and FuncArgs (tokens: Token list): Token list * AST =
    
    let rec Accumulate (tokens: Token list) (accumulator: AST list): Token list * AST list =
        match tokens with
        | [] -> tokens, accumulator
        | IDENTIFIER _ :: SEP :: tail -> Accumulate tail (accumulator @ [snd (Identifier tokens)])
        | Expr tokens  ->
            let rem, expr = Expr tokens
            Accumulate rem (accumulator @ [expr])
        | R_PAR :: tail -> tail, accumulator
        | _ -> raise(UnexpectedToken
                          $"Unexpected token encountered. Expected IDENTIFIER, EXPR, OR R_PAR.\nFound: {tokens[0]}")
    match tokens with
    | L_PAR :: tail ->
        let rem, accum = Accumulate tail []
        rem, { token = ARGS; decoration = "ArgTree"; children = accum }
    | _ -> raise( UnexpectedToken $"Unexpected token encountered. Expected L_PAR.\nFound: {tokens[0]}")
    