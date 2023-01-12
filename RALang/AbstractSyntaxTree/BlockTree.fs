module Transpiler.AbstractSyntaxTree.BlockTree

open Transpiler.Lexer.Token

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.AssignTree
open Transpiler.AbstractSyntaxTree.DeclTree
open Transpiler.AbstractSyntaxTree.ExprTree
open Transpiler.AbstractSyntaxTree.FuncCallTree

(*
    <statement> ::= <assign> | <decl>
    <condition> ::= <expr>
    
    <block> ::= (<statement>|<conditional>|<function_call>){<block>} 

    <if-clause> ::= 'if' <condition> 'then' <block>
    <elif-clause> ::= 'elif' <condition> 'then' <block>
    <else-clause> ::= 'else' <block>
    
    <conditional> ::= <if-clause>{<elif-clause>}{<else-clause>}'fi'
    
    <while> ::= 'while' <condition> 'do' <block> 'od'
*)

/// <summary>
/// Attempts to create either a Declaration or Assigment AST depending on input.
/// </summary>
/// <param name="tokens">A token list.</param>
/// <returns>Unparsed tokens and an AST or None.</returns>
let rec Statement (tokens: Token list) : Token list * AST option =
    match tokens with
    | Declaration _ ->
        match Decl tokens with
        | rem, Some ast -> rem, Some ast 
        | _ -> tokens, None // propagate None upwards.
    | Assignment _ ->
        match Assign tokens with
        | rem, Some ast -> rem, Some ast
        | _ -> tokens, None // propagate None upwards.
    | _ -> tokens, None // propagate None upwards.
/// <summary>
/// A function that attempts create a Block AST. A Block AST can consist of loops, conditional, and statements.
/// </summary>
/// <param name="tokens">A token list.</param>
/// <returns>Unparsed tokens and a Block AST or None.</returns>
and Block (tokens: Token list) : Token list * AST option =
    let rec Accumulate (remaining: Token list) (accumulator: AST list) =
        match remaining with
        | [] | (OD | ELIF | ELSE | FI | END) :: _ -> remaining, Some accumulator // terminators
        | LINE_END :: tail -> Accumulate tail accumulator
        | WHILE :: _ ->
            match Loop remaining with
            | rem, Some ast -> Accumulate rem (accumulator @ [ast])
            | _ -> remaining, None // propagate None upwards.
        | IF :: _ ->
            match Conditional remaining with
            | rem, Some ast -> Accumulate rem (accumulator @ [ast])
            | _ -> remaining, None // propagate None upwards.
        | IDENTIFIER _ :: L_PAR :: _ ->
            match FuncCall remaining with
            | rem, Some ast -> Accumulate rem (accumulator @ [ast])
            | _ -> remaining, None // propagate None upwards.
        | Declaration _ | Assignment _ -> 
            match Statement remaining with
            | rem, Some ast -> Accumulate rem (accumulator @ [ast])
            | _ -> remaining, None // propagate None upwards.
        | _ -> remaining, Some accumulator
    
    // accumulate children and construct block ast
    match Accumulate tokens [] with
    | rem, Some children -> rem, Some {
            token = BLOCK
            decoration = "BlockTree"
            children = children
        }
    | _ -> tokens, None // propagate None upwards.
/// <summary>
/// A function that attempts to create a Conditional AST.
/// A Conditional AST consists of a Condition AST and Block AST.
/// </summary>
/// <param name="tokens">A token list.</param>
/// <returns>Unparsed tokens and a Conditional AST or None.</returns>
and Conditional (tokens: Token list) : Token list * AST option =
    let rec Accumulate (tokens: Token list) (accumulator : AST list): Token list * AST list option =
        match tokens with
        | IF | ELIF as tok :: tail ->
            // match condition which is an expression
            match Expr tail with
            | THEN :: tail, Some ifCondAST ->
                // condition followed by block and some terminator.
                match Block tail with
                | (ELIF | ELSE) :: _ as ifBlockRem, Some ifBlockAST ->
                     // construct if/elif ast and continue accumulating conditionals
                     Accumulate ifBlockRem (accumulator @ [{
                        token = tok
                        decoration = $"""{match tok with IF -> "If" | ELIF -> "Elif"}Tree"""
                        children = [ifCondAST; ifBlockAST]
                     }])
                | FI :: tail, Some ifBlockAST ->
                    // construct if/elif ast and stop accumulation
                    tail, Some [{
                        token = tok
                        decoration = $"""{match tok with IF -> "If" | ELIF -> "Elif"}Tree"""
                        children = [ifCondAST; ifBlockAST]
                     }]
                | _ -> tokens, None // propagate None upwards
            | _ -> tokens, None // propagate None upwards
        | ELSE :: tail ->
            match Block tail with
            | FI :: tail, Some elseBlockAST ->
                // construct else ast and stop accumulation
                tail, Some (accumulator @ [{
                    token = ELSE
                    decoration = "ElseTree"
                    children = [elseBlockAST]
                }])
            | _ -> tokens, None // propagate None upwards
        | _ -> tokens, None // propagate None upwards
    
    // accumulate children and construct conditional ast
    match Accumulate tokens [] with
    | rem, Some accum -> rem, Some {
            token = CONDITIONAL
            decoration = "ConditionalTree"
            children = accum
        }
    | _ -> tokens, None // propagate None upwards.

/// <summary>
/// A function that attempts to create a Loop AST.
/// A Loop AST consists of a Condition AST and Block AST.
/// </summary>
/// <param name="tokens">A token list.</param>
/// <returns>Unparsed tokens and a Loop AST or None.</returns>
and Loop (tokens: Token list) : Token list * AST option =
    match tokens with
    | WHILE :: tail ->
        // match condition which is an expression
        match Expr tail with
        | DO :: tail, Some condAST ->
            // condition followed by block and OD terminator
            match Block tail with
            | OD :: tail, Some blockAST -> tail, Some {
                     token = LOOP
                     decoration = "LoopTree"
                     children = [{
                         token = WHILE
                         decoration = "WhileTree"
                         children = [condAST; blockAST]
                     }]
                 }
            | _ -> tokens, None // propagate None upwards.
        | _ -> tokens, None // propagate None upwards.
    | _ -> tokens, None // propagate None upwards.