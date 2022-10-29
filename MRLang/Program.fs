open System.Collections.Generic
open System.Text.Json
open MRLang.AbstractSyntaxTree

open Lexer
open MRLang.AbstractSyntaxTree.ExprTree
// printfn "%A" (Lexer.Lex "93 * \"hello hello\" 74")

// let x = AST([], "hello").AppendChild(AST([], "world"))

let lexemes = Lexer.Lex "9* 5 ^ (2 + (8/4) * 2)"
printf $"%A{lexemes}\n"



printf $"\n\n\n"
let rec printer (ast : AST) =
    if ast.children.Length = 0 then
        printf $"{ast.token} "
    else
        printer(ast.children[0])
        printf $"{ast.token} "
        printer(ast.children[1])
        

// printf $"Is Bin Expression: {BinExprTree.IsBinExpr lexemes}\n"
// printer ast
// printf $"= {BinExprTree.TestExpr lexemes} \n"


// printf $"Is Bin Expression: {BinExprTree.IsBinExpr lexemes}\n"
// printer ast
if ExprTree.isExpr lexemes then
    printf $"= {ExprTree.ExprEval lexemes} \n"
    let ast = ExprTree.Expr lexemes
    printf $"{ast}\n"
    
    printf $"{JsonSerializer.Serialize(ast)}"
else
    printf $"Error detected."

// printf $"{ast}"