module Transpiler.Parser.Parser

open Transpiler.Lexer.Token

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.ExprTree
open Transpiler.AbstractSyntaxTree.DeclTree
open Transpiler.AbstractSyntaxTree.AssignTree


exception UnsupportedKeywordError of string
exception TokenMatchingError of string

let private Expect (tokenList: string list) (token : string) =
    let rec Search (index : int) : bool =
        if index >= tokenList.Length then
            false
        else
            match tokenList[index] with
            | "\n" -> false
            | _ when token = tokenList[index] -> true
            | _ -> Search (index + 1) 
   
    if not(Search 0) then
        raise (TokenMatchingError $"Parsing error encountered. Expected {token}.")
    else
        ()
    
let Parse (tokens: Token list) : AST =
    printf $"Initial: %A{tokens}\n"
    
    let SymbolMap = Set.empty<string>;
    
    let rec FindSplit (tokens: Token list) (cursor : int) : int =
        match tokens with
        | LINE_END :: _ ->
            cursor
        | _ :: tail ->
            FindSplit tail (cursor + 1)
    
    let rec Execute (tokens: Token list) (ast: AST) : AST =
        if tokens.Length = 0 then
            ast
        else
            let split = FindSplit tokens 0
            let line = tokens[..split - 1]
            printf $"Parser: split: {split} -- %A{line}\n"
            match line with
            | [] when tokens.Length <> 0 ->
                Execute tokens[split + 1..] ast
            | [] -> ast
            | _ when isExpr line ->
                Execute tokens[split + 1..] { ast with children = ast.children @ [Expr line] }
            | _ when isDecl line ->
                //should test for re-declarations
                Execute tokens[split + 1..] { ast with children = ast.children @ [Decl line] }
            | _ when isAssign line ->
                Execute tokens[split + 1..] { ast with children = ast.children @ [Assign line] }
            | _ ->
                ast
    
    { token = MAIN; children = [Execute tokens {
        token = BLOCK; children = []; decoration = "BlockTree"
    }]; decoration = "Main" }