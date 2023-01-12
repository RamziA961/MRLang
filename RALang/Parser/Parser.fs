module Transpiler.Parser.Parser

open Transpiler.Lexer.Token

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.FuncCallTree
open Transpiler.AbstractSyntaxTree.BlockTree
open Transpiler.AbstractSyntaxTree.FuncTree

let Parse (tokens: Token list) : AST =
    let rec ConstructProgramTree (tokens: Token list) (ast: AST) : AST =
        match tokens with
        | LINE_END :: tail -> ConstructProgramTree tail ast
        | [] -> ast
        | _ ->
            match ConstructBranch tokens ast with
            | rem, Some ast -> ConstructProgramTree rem ast
            | _, None -> raise(SyntaxError $"Syntax error detected. Unable to parse tokens.\nTOKEN DUMP: %A{tokens}.")
    and ConstructBranch (tokens: Token list) (ast: AST): Token list * AST option = ConstructStatement tokens ast
    and ConstructStatement (tokens: Token list) (ast: AST): Token list * AST option =
        match Statement tokens with
        | rem, Some branch -> rem, Some { ast with children = ast.children @ [branch] }
        | _, None -> ConstructFuncCall tokens ast
    and ConstructFuncCall (tokens: Token list) (ast: AST): Token list * AST option =
        match FuncCall tokens with
        | rem, Some branch -> rem, Some { ast with children = ast.children @ [branch] }
        | _, None -> ConstructConditional tokens ast
    and ConstructConditional (tokens: Token list) (ast: AST): Token list * AST option =
        match Conditional tokens with
        | rem, Some branch -> rem, Some { ast with children = ast.children @ [branch] }
        | _, None -> ConstructFuncDecl tokens ast
    and ConstructFuncDecl (tokens: Token list) (ast: AST): Token list * AST option =
        match FuncDecl tokens with
        | rem, Some branch -> rem,  Some { ast with children = ast.children @ [branch] }
        | _, None -> tokens, None
        
    
    ConstructProgramTree tokens { token = PROGRAM; decoration = "ProgramTree"; children = [] }