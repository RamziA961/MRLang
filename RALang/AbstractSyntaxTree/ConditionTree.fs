module Transpiler.AbstractSyntaxTree.ConditionTree

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.ExprTree
open Transpiler.Lexer.Token


let rec Collect (index: int) (accumulated: Token list) =
    match accumulated with
    | [] -> -1
    | (THEN | DO | LINE_END) :: _ -> index
    | _ :: tail -> Collect (index + 1) tail


let isCondition (accumulated : Token list) =
    let exprEnd = Collect 0 accumulated
    exprEnd <> -1 && isExpr accumulated[..exprEnd] 
    
let Condition (accumulated: Token list) : AST =
    {
        token = CONDITION
        decoration = "ConditionTree"
        children = [Expr accumulated[.. (Collect 0 accumulated)]]
    }