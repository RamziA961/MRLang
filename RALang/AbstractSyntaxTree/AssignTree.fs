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

/// <summary>
/// Active pattern that matches Assignments.
/// </summary>
/// <param name="tokens">A token list.</param>
/// <returns>Some if the first sequence of tokens is an assignment, otherwise None. </returns>
let (|Assignment|_|) (tokens: Token list) =
    match tokens with
    | IDENTIFIER _ :: MUTATE :: _ -> Some(tokens)
    | TYPE _ :: IDENTIFIER _ :: (ASSIGN | MUTATE) :: _ -> Some(tokens)
    | _ -> None

/// <summary>
/// Attempts to create an Assignment/Mutate AST. It can consist of a Declaration AST and
/// an Expression/String AST or an Identifier AST and an Expression/String AST.
/// </summary>
/// <param name="tokens">A token list.</param>
/// <returns>Unparsed token list and an Assign/Mutate AST or None.</returns>
let Assign (tokens: Token list) : Token list * AST option =
  
    let Assignment (tokens : Token list, ast : AST option) = 
        match tokens, ast with
        | _, None -> tokens, ast
        | ASSIGN | MUTATE as tok :: tail, Some ast ->
            // match tail with String or Expression
            let remTokens, expr = match tail with
                                  | S s :: tail -> tail, Some { token = S s; children = []; decoration = "CharTree" } 
                                  | _ -> Expr tail
            
            match expr with
            | Some expr -> remTokens, Some {
                    children = [ast; expr]
                    decoration = match tok with | ASSIGN -> "AssignTree" | MUTATE -> "MutateTree"
                    token = tok
                }
            | None -> tokens, None // propagate None upwards.
        | _ -> tokens, ast
    
    let NonTerminal (tokens: Token list) : Token list * AST option =
        match tokens with
        | TYPE _ :: _ -> Decl tokens // Create a Declaration AST
        | IDENTIFIER _ :: _ -> Identifier tokens  // Create an identifier AST
        | _ -> tokens, None
     
    (NonTerminal >> Assignment) tokens
    