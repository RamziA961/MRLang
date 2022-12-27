module Transpiler.AbstractSyntaxTree.BlockTree

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.AssignTree
open Transpiler.AbstractSyntaxTree.DeclTree
open Transpiler.AbstractSyntaxTree.ExprTree
open Transpiler.Lexer.Token

(*
    <statement> ::= <assign> | <decl>
    <condition> ::= <expr>
    
    <block> ::= (<statement>|<conditional>){<block>} 

    <if-clause> ::= 'if' <condition> 'then' <block>
    <elif-clause> ::= 'elif' <condition> 'then' <block>
    <else-clause> ::= 'else' <block>
    
    <conditional> ::= <if-clause>{<elif-clause>}{<else-clause>}'fi'
*)

let isStatement (accumulated: Token list) = isAssign accumulated || isDecl accumulated

let rec Statement (tokens: Token list) : Token list * AST =
    match tokens with
    | _ when isDecl tokens -> Decl tokens 
    | _ when isAssign tokens -> Assign tokens
    | _ ->
        raise (UnexpectedToken (
            $"Unexpected token encountered: {tokens[0]}. Expected ASSIGNMENT or DECLARATION.\n\n" +
            $"TOKEN DUMP:\n%A{tokens}"
        ))
and Block (tokens: Token list) : Token list * AST =
    let rec Accumulate (remaining: Token list) (accumulator: AST list) =
        match remaining with
        | [] | (OD | ELIF | ELSE | FI | END) :: _ -> remaining, accumulator
        | LINE_END :: tail -> Accumulate tail accumulator
        | WHILE :: _ ->
            let remTokens, loopAST = Loop remaining
            Accumulate remTokens (accumulator @ [loopAST])
        | IF :: _ ->
            let remTokens, condAST = Conditional remaining
            Accumulate remTokens (accumulator @ [condAST])
        | (TYPE _ | IDENTIFIER _) :: _ -> 
            let remTokens, statementAST = Statement remaining     
            Accumulate remTokens (accumulator @ [statementAST])
        | _ -> remaining, accumulator
            
    let remTokens, children = Accumulate tokens []   
    remTokens, {
        token = BLOCK
        decoration = "BlockTree"
        children = children
    }
and Conditional (tokens: Token list) : Token list * AST =
    let rec Accumulate (tokens: Token list) (accumulator : AST list): Token list * AST list =
        match tokens with
        | (IF | ELIF) :: tail ->
            let ifCondRem, ifCondAST = Expr tail
            match ifCondRem with
            | THEN :: tail ->
                let ifBlockRem, ifBlockAST = Block tail
                let ifAST = {
                    token = tokens[0]
                    decoration = $"""{match tokens[0] with IF -> "If" | ELIF -> "Elif"}Tree"""
                    children = [ifCondAST; ifBlockAST]
                }
                
                match ifBlockRem with
                | ELIF :: _ -> Accumulate ifBlockRem (accumulator @ [ifAST])
                | ELSE :: _ -> Accumulate ifBlockRem (accumulator @ [ifAST])
                | FI :: tail -> tail, [ifAST]
                | _ ->  raise (
                        UnexpectedToken $"Unexpected token encountered: {ifBlockRem[0]}. Expected FI, ELIF, or ELSE."
                    )
            | _ -> raise (UnexpectedToken $"Unexpected token encountered: {ifCondRem[0]}. Expected THEN.")
        | ELSE :: tail ->
            let elseBlockRem, elseBlockAST = Block tail
            match elseBlockRem with
            | FI :: tail ->
                tail, (accumulator @ [{
                    token = tokens[0]
                    decoration = "elseTree"
                    children = [elseBlockAST]
                }])
            | _ -> raise (UnexpectedToken $"Unexpected token encountered: {elseBlockRem[0]}. Expected FI.")
        | _ -> raise(UnexpectedToken $"Unexpected token encountered: {tokens[0]}. Expected IF, FI, ELIF, or ELSE.")
    match tokens with
    | IF :: _ ->
        let remTokens, asts =  Accumulate tokens []
        remTokens, {
            token = CONDITIONAL
            decoration = "conditionalTree"
            children = asts
        }
    | _ -> raise (UnexpectedToken $"Unexpected token encountered: {tokens[0]}. Expected IF.")
and Loop (tokens: Token list) : Token list * AST =
    match tokens with
    | WHILE :: tail ->
        let condRem, condAST = Expr tail
        match condRem with
        | DO :: tail ->
            let blockRem, blockAST = Block tail
            match blockRem with
            | OD :: tail ->
                tail, {
                    token = LOOP
                    decoration = "loopTree"
                    children = [{
                        token = WHILE
                        decoration = "whileTree"
                        children = [condAST; blockAST]
                    }]
                }
            | _ -> raise (UnexpectedToken $"Unexpected token encountered: {blockRem[0]}. Expected OD.")
        | _ -> raise (UnexpectedToken $"Unexpected token encountered: {condRem[0]}. Expected DO.")
    | _ ->  raise (UnexpectedToken $"Unexpected token encountered: {tokens[0]}. Expected WHILE.")