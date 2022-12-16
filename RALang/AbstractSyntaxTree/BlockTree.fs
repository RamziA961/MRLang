module Transpiler.AbstractSyntaxTree.BlockTree

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.ConditionTree
open Transpiler.AbstractSyntaxTree.StatementTree

open Transpiler.AbstractSyntaxTree.AssignTree
open Transpiler.AbstractSyntaxTree.DeclTree
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


// let rec Conditional (accumulated: Token list) : AST = (Clause >> ConditionalTree) accumulated
// and ConditionalTree (accumulated: Token list, ast : AST) =
//     match accumulated with
//     | _ -> ast
// and Clause (accumulated: Token list) = (Block >> ClauseTree) accumulated
// and ClauseTree (accumulated: Token list, ast : AST) =
//     match accumulated with
//     | _ -> (accumulated, ast)
// and Block (accumulated: Token list) : Token list * AST =
//     match accumulated with
//     | _ -> ()

// let rec Block (accumulated: Token list, ast : AST): AST =
//     match accumulated with
//     | IF :: tail -> { ast with children = ast.children @ [Conditional accumulated] }
//     | (ELIF | ELSE | FI) :: tail -> ast
//     | _ when isStatement accumulated ->
//         
//         List.partition 
//         
//         { ast with children = ast.children @ [StatementTree accumulated] }
//       
// and IfClause (accumulated: Token list, ast : AST): AST =
//     match accumulated with
//     | IF :: tail ->
//         let condEnd = Collect 0 accumulated
//         {
//             token = IF
//             decoration = "IfTree"
//             children = [Condition tail[..condEnd]; Block tail[condEnd..]]
//         }
//     | _ -> ast
// and ElifClause (accumulated: Token list, ast : AST): AST =
          
    //       
    //     
    // | ELIF | ELSE | FI ->
            
    
            
    
    // and ElifClause (accumulated: Token list): AST =
    // and ElseClause (accumulated: Token list): AST =       

// let rec Conditional (accumulated: Token list) : AST =
//     {token = CONDITIONAL; decoration = "ConditionalTree"; children = [
//         match accumulated with
//         | IF :: tail ->
//             let conditionEnd = Collect 0 accumulated
//             {
//                 token = IF
//                 decoration = "IfTree"
//                 children = [Condition tail[..conditionEnd]; Block tail[conditionEnd..]]
//             }
//         | ELIF :: tail ->
//             let conditionEnd = Collect 0 accumulated
//             {
//                 token = ELIF
//                 decoration = "ElifTree"
//                 children = [Condition tail[..conditionEnd]; Block tail[conditionEnd..]]
//             }
//         | ELSE :: tail ->
//             {
//                 token = ELIF
//                 decoration = "ElifTree"
//                 children = [Block tail]
//             }    
//     ]}
// and Block (accumulated: Token list) : AST =
//     match accumulated with
//     | IF :: _ -> Conditional accumulated
//     | THEN :: tail -> {
//         token = BLOCK
//         decoration = "BlockTree"
//         children = []
//     }
//     | FI ->

let isStatement (accumulated: Token list) = isAssign accumulated || isDecl accumulated


let isBlock (accumulated: Token list) = true
let isConditional (accumulated: Token list) = true


let rec Statement (accumulated: Token list) =
    match accumulated with
    | _ when isDecl accumulated -> Decl accumulated 
    | _ when isAssign accumulated -> Assign accumulated
    | _ -> raise (UnexpectedToken $"Unexpected tokens encountered: %A{accumulated}")
and Block (accumulated: Token list) (ast : AST) =
    
    match accumulated with
    | IF  :: tail -> // [ CONDITIONAL ; BLOCK ]
        Conditional accumulated
    | ELIF :: tail -> // TERMINAL -- [ CONDITIONAL ; BLOCK ]
    
    | ELSE :: tail -> // TERMINAL -- [ BLOCK ] 
        
    | FI :: tail -> // TERMINAL
        
    | _  -> // (ASSUME STATEMENT) TERMINAL
    
    
    {
        token = BLOCK
        decoration = "BlockTree"
        children = []
    }
 
and Conditional (accumulated: Token list) =
    match accumulated with
    | IF -> 
      

    