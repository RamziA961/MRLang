module Transpiler.AbstractSyntaxTree.ExprTree

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.IdentifierTree
open Transpiler.Lexer.Token

(*
    BNF
    <expr> ::= <conjunction> {<disjunctionOp>}
    <disjunctionOp> ::= "||" <conjunction> <disjunctionOp> | <empty>
    
    <conjunction> ::= <equality> <conjunctionOp>
    <conjunctionOp> ::= "&&" <equality> <conjunctionOp> | <empty>
    
    <equality> ::= <comparison> <equalityOp>
    <equalityOp> ::= ("==" | "!=") <comparison> <equalityOp> | <empty>
    
    <comparison> ::= <term> <comparisonOp>
    <comparisonOp> ::= (">" | ">=" | "<" | "<=") <term> <comparisonOp> | <empty>
    
    <term> ::= <factor> <termOp> 
    <termOp> ::= ("+" | "-") <factor> <termOp> | <empty>
    
    <factor> ::= <exponent> <factorOp>
    <factorOp> ::= ("*" | "/") <exponent> <factorOp> | <empty>
    
    <exponent> ::= <primary> <exponentOp>
    <exponentOp> ::= "^" <primary> <exponentOp>
    
    <primary> ::= <int> | <float> | <bool> | "(" <expr> ")"
*)

/// <summary>
/// Determine whether given sequence of tokens are an expression.
/// </summary>
/// <param name="accumulatedTokens">A token list.</param>
/// <returns>Boolean indicating whether the list of tokens is an expression.</returns>
let isExpr (accumulatedTokens: Token list) : bool =
    let rec Expression(tokens: Token list) = (Conjunction >> DisjunctionOperation) tokens
    and DisjunctionOperation (tokens : Token list, value: bool) =
        match tokens with
        | OR :: tail ->
            let remTokens, conjunction = Conjunction tail
            DisjunctionOperation(remTokens, value && conjunction)
        | _ -> (tokens, value)
    and Conjunction (tokens: Token list) = (Equality >> ConjunctionOperation) tokens
    and ConjunctionOperation (tokens: Token list, value : bool) =
        match tokens with
        | AND :: tail ->
            let remTokens, equality = Equality tail
            DisjunctionOperation(remTokens, value && equality)
        | _ -> (tokens, value)
    and Equality (tokens : Token list) = (Comparison >> EqualityOperation) tokens
    and EqualityOperation (tokens: Token list, value: bool) =
        match tokens with
        | (EQ | NEQ) :: tail ->
                let remTokens, comparison = Comparison tail
                EqualityOperation(remTokens, value && comparison)
        | _ -> (tokens, value)
    and Comparison (tokens: Token list) = (Term >> ComparisonOperation) tokens
    and ComparisonOperation (tokens: Token list, value: bool) =
        match tokens with
        | (GT | GEQ | LT | LEQ) :: tail ->
            let remTokens, term = Term tail
            TermOperation(remTokens, value && term)
        | _ -> (tokens, value)
    and Term (tokens: Token list) = (Factor >> TermOperation) tokens
    and TermOperation (tokens: Token list, value : bool) =
        match tokens with
        | (ADD | SUB) :: tail ->
            let remTokens, factor = Factor tail
            TermOperation(remTokens, value && factor)
        | _ -> (tokens, value)
    and Factor (tokens: Token list) = (Exponentiation >> FactorOperation) tokens
    and FactorOperation (tokens: Token list, value: bool) =
        match tokens with
        | (MUL | DIV) :: tail ->
            let remTokens, exponentiation = Exponentiation tail
            FactorOperation(remTokens, value && exponentiation)
        | _ -> (tokens, value)
    and Exponentiation (tokens: Token list) = (Primary >> ExponentiationOperation) tokens
    and ExponentiationOperation (tokens : Token list, value : bool) =
        match tokens with
        | EXP :: tail ->
            let remTokens, primary = Primary tail
            ExponentiationOperation(remTokens, value && primary)
        | _ -> (tokens, value)
    and Primary (tokens: Token list) : Token list * bool =
        match tokens with
        | [] -> (tokens, false)
        | NOT :: tail ->
            let remTokens, primary = Primary tail
            (remTokens, primary)
        | IDENTIFIER _ :: tail -> (tail, true)
        | B _ :: tail -> (tail, true)
        | I _ :: tail -> (tail, true)
        | F _ :: tail -> (tail, true)
        | L_PAR :: tail -> 
            let remTokens, value = Expression tail
            match remTokens with
            | R_PAR :: tail -> (tail, value)
            | _ -> (tail, false)
        | LINE_END :: tail -> (tail, true)
        | _ :: tail -> (tail, false)
   
    snd (Expression accumulatedTokens) 

let (|Expr|_|) (tokens: Token list) = if isExpr tokens then Some(tokens) else None      


/// <summary>
/// Converts a list of tokens into an expression AST. If conversion fails None is returned
/// </summary>
/// <param name="accumulatedTokens">A token list.</param>
/// <returns>Unparsed token list and an Expression AST or None.</returns>
/// <exception cref="SyntaxError">
/// Throws an exception when a number, parenthetical, identifier, or negation is expected but not found.
/// </exception>
let Expr(accumulatedTokens : Token list) : Token list * AST option =
    
    // Expression AST generator. Highest priority operations are processed first.
    let rec Expression (tokens : Token list) = (Conjunction >> DisjunctionOperation) tokens
    and DisjunctionOperation (tokens: Token list, ast: AST option) : Token list * AST option =
        match tokens, ast with
        | _, None -> tokens, ast // propagate None upwards.
        | OR :: tail, Some ast -> 
            // check for higher priority operation (conjunction)
            let remTokens, conjunction = Conjunction tail
            match conjunction with
            | Some conjunction ->
                DisjunctionOperation (remTokens, Some {
                    token = OR
                    decoration = "OrTree"
                    children = [ast; conjunction]
                })
            | None -> tokens, conjunction
        | _ -> (tokens, ast) 
    and Conjunction (tokens : Token list) = (Equality >> ConjunctionOperation) tokens
    and ConjunctionOperation (tokens: Token list, ast : AST option) : Token list * AST option =
        match tokens, ast with
        | _, None -> tokens, ast // propagate None upwards.
        | AND :: tail, Some ast ->
            // check for higher priority operation (equality)
            let remTokens, equality = Equality tail
            match equality with
            | Some equality -> 
                ConjunctionOperation(remTokens, Some {
                    token = AND
                    decoration = "AndTree"
                    children = [ast; equality]
                })
            | None -> tokens, equality
        | _ -> tokens, ast
    and Equality (tokens : Token list) = (Comparison >> EqualityOperation) tokens
    and EqualityOperation (tokens: Token list, ast: AST option) : Token list * AST option =
        match tokens, ast with
        | _, None -> tokens, ast // propagate None upwards.
        | EQ | NEQ as tok :: tail, Some ast ->
            // check for higher priority operation (Comparison or Relational operation)
            let remTokens, comparison = Comparison tail
            match comparison with
            | Some comparison -> 
                EqualityOperation (remTokens, Some {
                    token = tok
                    decoration = match tok with | EQ -> "EqualsTree" | NEQ -> "NotEqualsTree"
                    children = [ast; comparison]
                })
            | None -> tokens, comparison
        | _ -> tokens, ast
    and Comparison (tokens: Token list) = (Term >> ComparisonOperation) tokens
    and ComparisonOperation (tokens: Token list, ast: AST option) : Token list * AST option =
        match tokens, ast with
        | _, None -> tokens, ast // propagate None upwards.
        | GT | GEQ | LT | LEQ as tok :: tail, Some ast ->
            // check for higher priority operation (Term or Addition/Subtraction)
            let remTokens, term = Term tail
            match term with
            | Some term ->
                TermOperation(remTokens, Some {
                    token = tok
                    decoration = match tok with | GT -> "GreaterThanTree"
                                                | GEQ -> "GreaterEqTree"
                                                | LT -> "LessThanTree"
                                                | LEQ -> "LessEqTree"
                    children = [ast; term]
                })
            | None -> tokens, term
        | _ -> tokens, ast
    and Term (tokens: Token list) = (Factor >> TermOperation) tokens
    and TermOperation (tokens: Token list, ast : AST option) : Token list * AST option =
        match tokens, ast with
        | _, None -> tokens, ast // propagate None upwards.
        | ADD | SUB as tok :: tail, Some ast ->
            // check for higher priority operation (Factor or Multiplication/Division operation)
            let remTokens, factor = Factor tail
            match factor with
            | Some factor ->
                TermOperation (remTokens, Some {
                    token = tok
                    decoration = match tok with | ADD -> "AddTree" | SUB -> "SubTree"
                    children = [ast; factor]
                })
            | None -> tokens, factor
        | _ -> tokens, ast   
    and Factor (tokens: Token list) = (Exponentiation >> FactorOperation) tokens
    and FactorOperation (tokens: Token list, ast: AST option) : Token list * AST option =
        match tokens, ast with
        | _, None -> tokens, ast // propagate None upwards.
        | MUL | DIV as tok :: tail, Some ast ->
            // check for higher priority operation (Exponentiation)
            let remTokens, exponentiation = Exponentiation tail
            match exponentiation with
            | Some exponentiation ->
                FactorOperation (remTokens, Some {
                    token = tok
                    decoration = match tok with | MUL -> "MulTree" | DIV -> "DivTree"
                    children = [ast; exponentiation]
                })
            | None -> tokens, exponentiation
        | _ -> tokens, ast
    and Exponentiation (tokens: Token list) = (Primary >> ExponentiationOperation) tokens
    and ExponentiationOperation (tokens : Token list, ast : AST option) : Token list * AST option =
        match tokens, ast with
        | _, None -> tokens, ast // propagate None upwards.
        | EXP :: tail, Some ast ->
            // check for higher priority value (Primary)
            let remTokens, primary = Primary tail
            match primary with
            | Some primary ->
                ExponentiationOperation (remTokens, Some {
                    token = EXP
                    decoration = "PowTree"
                    children = [ast; primary]
                })
            | None -> tokens, primary
        | _ -> tokens, ast
    and Primary (tokens: Token list) : Token list * AST option =
        match tokens with
        | NOT :: tail ->
            let remTokens, primary = Primary tail
            match primary with
            | Some primary -> remTokens, Some {
                    token = NOT
                    decoration = "NotTree"
                    children = [primary]
                }
            | None -> tokens, primary
        | IDENTIFIER _ :: _ -> Identifier tokens
        | B b :: tail ->
            tail, Some {
                token = B b
                decoration = "BoolTree"
                children = []
            }
        | I i :: tail  ->
            tail, Some {
                token = I i
                decoration = "IntTree"
                children = []
            }
        | F f :: tail ->
            tail, Some {
                token = F f
                decoration = "FloatTree"
                children = []
            }
        | L_PAR :: tail ->
            // construct higher priority Expression.
            let remTokens, value = Expression tail
            match remTokens, value with
            | _, None -> tokens, value
            | R_PAR :: tail, Some value ->
                    tail, Some { value with children =
                                    [{children = []; token = L_PAR; decoration = "ParenthesisTree"}] @
                                    value.children @
                                    [{children = []; token = R_PAR; decoration = "ParenthesisTree"}]
                   }
            | _ -> tokens, value
        | _ -> raise (SyntaxError ("Syntax error encountered. Expected Primitive, L_PAR, R_PAR, or NOT." +
                      $"\nTOKEN DUMP: %A{tokens}."))
    
    Expression accumulatedTokens