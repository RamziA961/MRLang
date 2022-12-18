module Transpiler.AbstractSyntaxTree.AssignTree

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.Lexer.Token
open Transpiler.AbstractSyntaxTree.ExprTree
open Transpiler.AbstractSyntaxTree.DeclTree
open Transpiler.AbstractSyntaxTree.IdentifierTree

(*
    <decl> := <type><identifier>
    <assignment> := <decl>'='(<expr>|<string>) | <identifier>':='(<expr>|<string>) 
*)

let isAssign (tokens : Token list) : bool =
    match tokens with
    | TYPE _ :: _ ->
        isDecl tokens[0..1]
        && match tokens[2] with
            | ASSIGN | MUTATE ->
                true
            | _ -> false
        && isExpr tokens[3..]
    | IDENTIFIER _ :: _ ->
        match tokens[1] with
        | MUTATE ->
            true
        | _ -> false
        && isExpr tokens[2..]   
    | _ -> false
            
           
let Assign (tokens: Token list) : Token list * AST =
    let Assignment (tokens : Token list, ast : AST) = 
        match tokens with
        | ASSIGN :: tail ->
            let remTokens, expr = Expr tail
            (remTokens, {
                children = [ast; expr]
                decoration = "AssignTree"
                token = tokens.Head
            })
        | MUTATE :: tail ->
            let remTokens, expr = Expr tail
            (remTokens, {
                children = [ast; expr]
                decoration = "MutateTree"
                token = tokens.Head
            })
        | _ -> raise(UnexpectedToken $"Unexpected token encountered: {tokens[0]}. Expected ASSIGN or MUTATE.")
    
    let NonTerminal (tokens: Token list) : Token list * AST =
        match tokens with
        | TYPE _ :: _ -> Decl tokens
        | IDENTIFIER _ :: _ -> Identifier tokens
        | _ -> raise(UnexpectedToken $"Unexpected token encountered: {tokens[0]}. Expected TYPE or IDENTIFIER.")
     
    (NonTerminal >> Assignment) tokens
    