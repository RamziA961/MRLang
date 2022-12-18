module Transpiler.AbstractSyntaxTree.ExprTree

open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.AbstractSyntaxTree.IdentifierTree
open Transpiler.Lexer.Token

let private IsBinaryOperator (s: string) : bool = BOpTokenMap.ContainsKey s
let private IsRelOperator(s : string) : bool = ROpTokenMap.ContainsKey s

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
    
    // if accumulatedTokens.IsEmpty then
    //     false
    // else
    //     let tokens, status = Expression accumulatedTokens
    //     tokens.IsEmpty && status
    snd (Expression accumulatedTokens) 
            
    
let Expr(accumulatedTokens : Token list) : Token list * AST =
    let rec Expression (tokens : Token list) = (Conjunction >> DisjunctionOperation) tokens
    and DisjunctionOperation (tokens: Token list, ast: AST)  =
        match tokens with
        | OR :: tail ->
            let remTokens, conjunction = Conjunction tail 
            DisjunctionOperation (remTokens, {
                token = OR
                decoration = "OrTree"
                children = [ast; conjunction]
            })
        | _ -> (tokens, ast) 
    and Conjunction (tokens : Token list) = (Equality >> ConjunctionOperation) tokens
    and ConjunctionOperation (tokens: Token list, ast : AST) =
        match tokens with
        | AND :: tail ->
            let remTokens, equality = Equality tail
            ConjunctionOperation(remTokens, {
                token = AND
                decoration = "AndTree"
                children = [ast; equality]
            })
        | _ -> (tokens, ast)
    and Equality (tokens : Token list) = (Comparison >> EqualityOperation) tokens
    and EqualityOperation (tokens: Token list, ast: AST) =
        match tokens with
        | EQ :: tail ->
                let remTokens, comparison = Comparison tail
                EqualityOperation(remTokens, {
                    token = EQ
                    decoration = "EqualsTree"
                    children = [ast; comparison]
                })
        | NEQ :: tail ->
                let remTokens, comparison = Comparison tail
                EqualityOperation(remTokens, {
                    token = NEQ
                    decoration = "NEqualsTree"
                    children = [ast; comparison]
                })
        | _ -> (tokens, ast)
    and Comparison (tokens: Token list) = (Term >> ComparisonOperation) tokens
    and ComparisonOperation (tokens: Token list, ast: AST) =
        match tokens with
        | GT :: tail ->
            let remTokens, term = Term tail
            TermOperation(remTokens, {
                token = GT
                decoration = "GreaterThanTree"
                children = [ast; term]
            })
        | GEQ :: tail ->
            let remTokens, term = Term tail
            TermOperation(remTokens, {
                token = GEQ
                decoration = "GreaterEqualsTree"
                children = [ast; term]
            })
        | LT :: tail ->
            let remTokens, term = Term tail
            TermOperation(remTokens, {
                token = LT
                decoration = "LessThanTree"
                children = [ast; term]
            })
        | LEQ :: tail ->
            let remTokens, term = Term tail
            TermOperation(remTokens, {
                token = LEQ
                decoration = "LessEqualsTree"
                children = [ast; term]
            })
        | _ -> (tokens, ast)
    and Term (tokens: Token list) = (Factor >> TermOperation) tokens
    and TermOperation (tokens: Token list, ast : AST) =
        match tokens with
        | ADD :: tail ->
            let remTokens, factor = Factor tail
            TermOperation (remTokens, {
                token = ADD
                decoration = "AddTree"
                children = [ast; factor]
            })
        | SUB :: tail ->
            let remTokens, factor = Factor tail
            TermOperation (remTokens, {
                token = SUB
                decoration = "SubTree"
                children = [ast; factor]
            })
        | _ -> (tokens, ast)    
    and Factor (tokens: Token list) = (Exponentiation >> FactorOperation) tokens
    and FactorOperation (tokens: Token list, ast: AST) =
        match tokens with
        | MUL :: tail ->
            let remTokens, exponentiation = Exponentiation tail
            FactorOperation (remTokens, {
                token = MUL
                decoration = "MulTree"
                children = [ast; exponentiation]
            })
        | DIV :: tail ->
            let remTokens, exponentiation = Exponentiation tail
            FactorOperation (remTokens, {
                token = DIV
                decoration = "DivTree"
                children = [ast; exponentiation]
            })
        | _ -> (tokens, ast)
    and Exponentiation (tokens: Token list) = (Primary >> ExponentiationOperation) tokens
    and ExponentiationOperation (tokens : Token list, ast : AST) =
        match tokens with
        | EXP :: tail ->
            let remTokens, primary = Primary tail
            ExponentiationOperation (remTokens, {
                token = EXP
                decoration = "PowTree"
                children = [ast; primary]
            })
        | _ -> (tokens, ast)
    and Primary (tokens: Token list) : Token list * AST =
        match tokens with
        | IDENTIFIER _ :: _ ->
             Identifier tokens
        | B b :: tail ->
            (tail, {
                token = B b
                decoration = "BoolTree"
                children = []
            })
        | I i :: tail  ->
            (tail, {
                token = I i
                decoration = "IntTree"
                children = []
            })
        | F f :: tail ->
            (tail, {
                token = F f
                decoration = "FloatTree"
                children = []
            })
        | L_PAR :: tail -> 
            let remTokens, value = Expression tail
            match remTokens with
            | R_PAR :: tail ->
                    (tail, {
                        value with children =
                                    [{children = []; token = L_PAR; decoration = "ParenthesisTree"}] @
                                    value.children @
                                    [{children = []; token = R_PAR ;decoration = "ParenthesisTree"}]
                   })
            | _ -> raise (TokenMatchingError "Mismatched number of parentheses encountered.")
        
    Expression accumulatedTokens
            
            
let ExprEval (accumulatedTokens: Token list) : Token =
    let rec Expression (tokens: Token list) = (Conjunction >> DisjunctionOperation) tokens
    and DisjunctionOperation (tokens : Token list, value : Token) =
        match tokens with
        | OR :: tail ->
            let remTokens, conjunction = Conjunction tail
            match value, conjunction with
            | B v, B c ->
                DisjunctionOperation(remTokens, B (v || c))
            | _ -> raise (InvalidOperation "The conjunction operation with numerical values is unsupported.")
        | _ -> (tokens, value)
    and Conjunction (tokens: Token list) = (Equality >> ConjunctionOperation) tokens
    and ConjunctionOperation (tokens : Token list, value : Token) =
        match tokens with
        | AND :: tail ->
            let remTokens, equality = Equality tail
            match value, equality with
            | B v, B e ->
                ConjunctionOperation(remTokens, B (v && e))
            | _ -> raise (InvalidOperation "The disjunction operation with numerical values is unsupported.")
        | _ -> (tokens, value)
    and Equality (tokens : Token list) = (Comparison >> EqualityOperation) tokens
    and EqualityOperation (tokens: Token list, value: Token) =
        match tokens with
        | EQ :: tail ->
                let remTokens, comparison = Comparison tail
                EqualityOperation(remTokens, B (value = comparison))
        | NEQ :: tail ->
                let remTokens, comparison = Comparison tail
                EqualityOperation(remTokens, B (value <> comparison))
        | _ -> (tokens, value)
    and Comparison (tokens: Token list) = (Term >> ComparisonOperation) tokens
    and ComparisonOperation (tokens: Token list, value: Token) =
        match tokens with
        | GT :: tail ->
            let remTokens, term = Term tail
            TermOperation(remTokens, B (value > term))
        | GEQ :: tail ->
            let remTokens, term = Term tail
            TermOperation(remTokens, B (value >= term))
        | LT :: tail ->
            let remTokens, term = Term tail
            TermOperation(remTokens, B (value < term))
        | LEQ :: tail ->
            let remTokens, term = Term tail
            TermOperation(remTokens, B (value <= term))
        | _ -> (tokens, value)
    and Term (tokens: Token list) = (Factor >> TermOperation) tokens
    and TermOperation (tokens: Token list, value : Token) =
        match tokens with
        | ADD :: tail ->
            let remTokens, factor = Factor tail
            match value, factor with
            | I v, I f ->
                TermOperation (remTokens, I (v + f))
            | I v, F f  ->
                TermOperation (remTokens, F (float v + f))
            | F v, I f  ->
                TermOperation (remTokens, F (v + float f))
            | F v, F f ->
                TermOperation (remTokens, F (v + f))
            | _ -> raise (InvalidOperation "Boolean addition is unsupported.")
        | SUB :: tail ->
            let remTokens, factor = Factor tail
            match value, factor with
            | I v, I f ->
                TermOperation (remTokens, I (v - f))
            | I v, F f ->
                TermOperation (remTokens, F (float v - f))
            | F v, I f  ->
                TermOperation (remTokens, F (v - float f))
            | F v, F f ->
                TermOperation (remTokens, F (v - f))
            | _ -> raise (InvalidOperation "Boolean subtraction is unsupported.")
        | _ -> (tokens, value)
    and Factor (tokens: Token list) = (Exponentiation >> FactorOperation) tokens
    and FactorOperation (tokens: Token list, value: Token) =
        match tokens with
        | MUL :: tail ->
            let remTokens, exponentiation = Exponentiation tail
            match value, exponentiation with
            | I v, I e ->
                FactorOperation (remTokens, I (v * e))
            | I v, F e  ->
                FactorOperation (remTokens, F (float v * e))
            | F v, I e  ->
                FactorOperation (remTokens, F (v * float e))
            | F v, F e ->
                FactorOperation (remTokens, F (v * e))
            | _ -> raise (InvalidOperation "Boolean multiplication is unsupported.")
        | DIV :: tail ->
            let remTokens, exponentiation = Exponentiation tail
            match value, exponentiation with
            | I v, I e ->
                FactorOperation (remTokens, I (v / e))
            | I v, F e  ->
                FactorOperation (remTokens, F (float v / e))
            | F v, I e  ->
                FactorOperation (remTokens, F (v / float e))
            | F v, F e ->
                FactorOperation (remTokens, F (v / e))
            | _ -> raise (InvalidOperation "Boolean division is unsupported.")
        | _ -> (tokens, value)
    and Exponentiation (tokens: Token list) = (Primary >> ExponentiationOperation) tokens
    and ExponentiationOperation (tokens : Token list, value : Token) =
        match tokens with
        | EXP :: tail ->
            let remTokens, primary = Primary tail
            match value, primary with
            | I v, I f ->
                ExponentiationOperation (remTokens, I (pown v  f))
            | I v, F f ->
                ExponentiationOperation (remTokens, F (float v ** f))
            | F v, I f ->
                ExponentiationOperation (remTokens, F (v ** float f))
            | F v, F f ->
                ExponentiationOperation (remTokens, F (v ** f))
            | _ -> raise (InvalidOperation "Boolean exponentiation is unsupported.")
        | _ -> (tokens, value)
    and Primary (tokens: Token list) : Token list * Token =
        match tokens with
        | IDENTIFIER id :: tail ->
            (tail, IDENTIFIER id)
        | B b :: tail ->
            (tail, B b)
        | I i :: tail ->
            (tail, I i)
        | F f :: tail ->
            (tail, F f)
        | L_PAR :: tail -> 
            let remTokens, value = Expression tail
            match remTokens with
            | R_PAR :: tail -> (tail, value)
            | _ -> raise (TokenMatchingError "Mismatched number of parentheses encountered.")
            
    snd (Expression accumulatedTokens)
