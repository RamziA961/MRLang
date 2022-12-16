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
            
        
        
let Assign (tokens: Token list) : AST =
    
    let Assignment (ast : AST, tokens : Token list) = 
        match tokens with
        | ASSIGN :: tail ->
            {
                children = [ast; Expr tail]
                decoration = "AssignTree"
                token = tokens.Head
            }
        | MUTATE :: tail ->
            {
                children = [ast; Expr tail]
                decoration = "MutateTree"
                token = tokens.Head
            }
        | _ -> raise(UnexpectedToken "Unexpected token encountered.")
    
    let NonTerminal (tokens: Token list) =
        match tokens with
        | TYPE _ :: _ ->
            (Decl tokens[0..2], tokens[2..])
        | IDENTIFIER _ :: _ ->
            (Identifier tokens[0], tokens[1..])
        | _ -> raise(UnexpectedToken "Unexpected token encountered.")
     
    (NonTerminal >> Assignment) tokens
    