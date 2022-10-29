module MRLang.AbstractSyntaxTree.ExprTree

open MRLang.Token

open System


module ExprTree =
    exception TokenMatchingError of string
    exception SyntaxError of string
    exception InvalidOperation of string
    
    type Primitive =
         | I of int
         | F of float
         | B of bool
         
    let private IsInteger (s: string) : bool = s |> Seq.forall Char.IsDigit
    let private IsFloat (s: string) : bool =
        Seq.forall (fun x ->  Char.IsDigit x || x = '.') s &&
        Seq.length (Seq.filter (fun x -> x = '.') s) = 1 &&
        Seq.last s <> '.' &&
        Seq.head s <> '.'
        
    let private IsBinaryOperator (s: string) : bool = Token.BinaryOperator.Contains s
    let private IsRelOperator(s : string) : bool = Token.RelationalOperator.Contains s
    
    
    let isExpr (accumulatedTokens: string list) : bool =
        let rec Expression(tokens: string list) = (Conjunction >> DisjunctionOperation) tokens
        and DisjunctionOperation (tokens : string list, value: bool) =
            match tokens with
            | "||" :: tail ->
                let remTokens, conjunction = Conjunction tail
                DisjunctionOperation(remTokens, value && conjunction)
            // | _ :: tail when not (List.isEmpty tail) -> (tokens, false)  
            | _ -> (tokens, value)
        and Conjunction (tokens: string list) = (Equality >> ConjunctionOperation) tokens
        and ConjunctionOperation (tokens: string list, value : bool) =
            match tokens with
            | "&&" :: tail ->
                let remTokens, equality = Equality tail
                DisjunctionOperation(remTokens, value && equality)
            | _ -> (tokens, value)
        and Equality (tokens : string list) = (Comparison >> EqualityOperation) tokens
        and EqualityOperation (tokens: string list, value: bool) =
            match tokens with
            | ("==" | "!=") :: tail ->
                    let remTokens, comparison = Comparison tail
                    EqualityOperation(remTokens, value && comparison)
            | _ -> (tokens, value)
        and Comparison (tokens: string list) = (Term >> ComparisonOperation) tokens
        and ComparisonOperation (tokens: string list, value: bool) =
            match tokens with
            | (">" | ">=" | "<" | "<=") :: tail ->
                let remTokens, term = Term tail
                TermOperation(remTokens, value && term)
            | _ -> (tokens, value)
        and Term (tokens: string list) = (Factor >> TermOperation) tokens
        and TermOperation (tokens: string list, value : bool) =
            match tokens with
            | ("+" | "-") :: tail ->
                let remTokens, factor = Factor tail
                TermOperation(remTokens, value && factor)
            | _ -> (tokens, value)
        and Factor (tokens: string list) = (Exponentiation >> FactorOperation) tokens
        and FactorOperation (tokens: string list, value: bool) =
            match tokens with
            | ("*" | "/") :: tail ->
                let remTokens, exponentiation = Exponentiation tail
                FactorOperation(remTokens, value && exponentiation)
            | _ -> (tokens, value)
        and Exponentiation (tokens: string list) = (Primary >> ExponentiationOperation) tokens
        and ExponentiationOperation (tokens : string list, value : bool) =
            match tokens with
            | "^" :: tail ->
                let remTokens, primary = Primary tail
                ExponentiationOperation(remTokens, value && primary)
            | _ -> (tokens, value)
        and Primary (tokens: string list) : string list * bool =
            match tokens with
            | ("T" | "F") :: tail ->
                (tail, true)
            | _ :: tail when IsInteger tokens.Head ->
                (tail, true)
            | _ :: tail when IsFloat tokens.Head ->
                (tail, true)
            | "(" :: tail -> 
                let remTokens, value = Expression tail
                match remTokens with
                | ")" :: tail -> (tail, value)
                | _ -> (tail, false)
            | _ :: tail -> (tail, false)
        
        if accumulatedTokens.IsEmpty then
            false
        else
            snd(Expression accumulatedTokens)
    
    let Expr(accumulatedTokens : string list) : AST =
        let rec Expression (tokens : string list) = (Conjunction >> DisjunctionOperation) tokens
        and DisjunctionOperation (tokens: string list, ast: AST) =
            match tokens with
            | "||" :: tail ->
                let remTokens, conjunction = Conjunction tail 
                DisjunctionOperation (remTokens, {
                    token = "||"
                    decoration = "OrTree"
                    children = [ast; conjunction]
                })
            | _ -> (tokens, ast) 
        and Conjunction (tokens : string list) = (Equality >> ConjunctionOperation) tokens
        and ConjunctionOperation (tokens: string list, ast : AST) =
            match tokens with
            | "&&" :: tail ->
                let remTokens, equality = Equality tail
                ConjunctionOperation(remTokens, {
                    token = "&&"
                    decoration = "AndTree"
                    children = [ast; equality]
                })
            | _ -> (tokens, ast)
        and Equality (tokens : string list) = (Comparison >> EqualityOperation) tokens
        and EqualityOperation (tokens: string list, ast: AST) =
            match tokens with
            | "==" :: tail ->
                    let remTokens, comparison = Comparison tail
                    EqualityOperation(remTokens, {
                        token = "=="
                        decoration = "EqualsTree"
                        children = [ast; comparison]
                    })
            | "!=" :: tail ->
                    let remTokens, comparison = Comparison tail
                    EqualityOperation(remTokens, {
                        token = "!="
                        decoration = "NEqualsTree"
                        children = [ast; comparison]
                    })
            | _ -> (tokens, ast)
        and Comparison (tokens: string list) = (Term >> ComparisonOperation) tokens
        and ComparisonOperation (tokens: string list, ast: AST) =
            match tokens with
            | ">" :: tail ->
                let remTokens, term = Term tail
                TermOperation(remTokens, {
                    token = ">"
                    decoration = "GreaterThanTree"
                    children = [ast; term]
                })
            | ">=" :: tail ->
                let remTokens, term = Term tail
                TermOperation(remTokens, {
                    token = ">="
                    decoration = "GreaterEqualsTree"
                    children = [ast; term]
                })
            | "<" :: tail ->
                let remTokens, term = Term tail
                TermOperation(remTokens, {
                    token = "<"
                    decoration = "LessThanTree"
                    children = [ast; term]
                })
            | "<=" :: tail ->
                let remTokens, term = Term tail
                TermOperation(remTokens, {
                    token = "<="
                    decoration = "LessEqualsTree"
                    children = [ast; term]
                })
            | _ -> (tokens, ast)
        and Term (tokens: string list) = (Factor >> TermOperation) tokens
        and TermOperation (tokens: string list, ast : AST) =
            match tokens with
            | "+" :: tail ->
                let remTokens, factor = Factor tail
                TermOperation (remTokens, {
                    token = "+"
                    decoration = "AddTree"
                    children = [ast; factor]
                })
            | "-" :: tail ->
                let remTokens, factor = Factor tail
                TermOperation (remTokens, {
                    token = "-"
                    decoration = "SubTree"
                    children = [ast; factor]
                })
            | _ -> (tokens, ast)    
        and Factor (tokens: string list) = (Exponentiation >> FactorOperation) tokens
        and FactorOperation (tokens: string list, ast: AST) =
            match tokens with
            | "*" :: tail ->
                let remTokens, exponentiation = Exponentiation tail
                FactorOperation (remTokens, {
                    token = "*"
                    decoration = "MulTree"
                    children = [ast; exponentiation]
                })
            | "/" :: tail ->
                let remTokens, exponentiation = Exponentiation tail
                FactorOperation (remTokens, {
                    token = "/"
                    decoration = "DivTree"
                    children = [ast; exponentiation]
                })
            | _ -> (tokens, ast)
        and Exponentiation (tokens: string list) = (Primary >> ExponentiationOperation) tokens
        and ExponentiationOperation (tokens : string list, ast : AST) =
            match tokens with
            | "^" :: tail ->
                let remTokens, primary = Primary tail
                ExponentiationOperation (remTokens, {
                    token = "^"
                    decoration = "PowTree"
                    children = [ast; primary]
                })
            | _ -> (tokens, ast)
        and Primary (tokens: string list) : string list * AST =
            match tokens with
            | ("T" | "F") :: tail ->
                (tail, {
                    token = tokens.Head
                    decoration = "BoolTree"
                    children = []
                })
            | _ :: tail when IsInteger tokens.Head ->
                (tail, {
                    token = tokens.Head
                    decoration = "IntTree"
                    children = []
                })
            | _ :: tail when IsFloat tokens.Head ->
                (tail, {
                    token = tokens.Head
                    decoration = "FloatTree"
                    children = []
                })
            | "(" :: tail -> 
                let remTokens, value = Expression tail
                match remTokens with
                | ")" :: tail -> (tail, value)
                | _ -> raise (TokenMatchingError "Mismatched number of parentheses encountered.")
        
        snd(Expression accumulatedTokens)
            
            
    let ExprEval (accumulatedTokens: string list) : Primitive =
        let rec Expression (tokens: string list) = (Conjunction >> DisjunctionOperation) tokens
        and DisjunctionOperation (tokens : string list, value : Primitive) =
            match tokens with
            | "||" :: tail ->
                let remTokens, conjunction = Conjunction tail
                match value, conjunction with
                | B v, B c ->
                    DisjunctionOperation(remTokens, B (v || c))
                | _ -> raise (InvalidOperation "The conjunction operation with numerical values is unsupported.")
            | _ -> (tokens, value)
        and Conjunction (tokens: string list) = (Equality >> ConjunctionOperation) tokens
        and ConjunctionOperation (tokens : string list, value : Primitive) =
            match tokens with
            | "&&" :: tail ->
                let remTokens, equality = Equality tail
                match value, equality with
                | B v, B e ->
                    ConjunctionOperation(remTokens, B (v && e))
                | _ -> raise (InvalidOperation "The disjunction operation with numerical values is unsupported.")
            | _ -> (tokens, value)
        and Equality (tokens : string list) = (Comparison >> EqualityOperation) tokens
        and EqualityOperation (tokens: string list, value: Primitive) =
            match tokens with
            | "==" :: tail ->
                    let remTokens, comparison = Comparison tail
                    EqualityOperation(remTokens, B (value = comparison))
            | "!=" :: tail ->
                    let remTokens, comparison = Comparison tail
                    EqualityOperation(remTokens, B (value <> comparison))
            | _ -> (tokens, value)
        and Comparison (tokens: string list) = (Term >> ComparisonOperation) tokens
        and ComparisonOperation (tokens: string list, value: Primitive) =
            match tokens with
            | ">" :: tail ->
                let remTokens, term = Term tail
                TermOperation(remTokens, B (value > term))
            | ">=" :: tail ->
                let remTokens, term = Term tail
                TermOperation(remTokens, B (value >= term))
            | "<" :: tail ->
                let remTokens, term = Term tail
                TermOperation(remTokens, B (value < term))
            | "<=" :: tail ->
                let remTokens, term = Term tail
                TermOperation(remTokens, B (value <= term))
            | _ -> (tokens, value)
        and Term (tokens: string list) = (Factor >> TermOperation) tokens
        and TermOperation (tokens: string list, value : Primitive) =
            match tokens with
            | "+" :: tail ->
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
            | "-" :: tail ->
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
        and Factor (tokens: string list) = (Exponentiation >> FactorOperation) tokens
        and FactorOperation (tokens: string list, value: Primitive) =
            match tokens with
            | "*" :: tail ->
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
            | "/" :: tail ->
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
        and Exponentiation (tokens: string list) = (Primary >> ExponentiationOperation) tokens
        and ExponentiationOperation (tokens : string list, value : Primitive) =
            match tokens with
            | "^" :: tail ->
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
        and Primary (tokens: string list) : string list * Primitive =
            match tokens with
            | ("T" | "F") :: tail ->
                (tail, B (tokens.Head = "T"))
            | _ :: tail when IsInteger tokens.Head ->
                (tail, I (int tokens.Head))
            | _ :: tail when IsFloat tokens.Head ->
                (tail, F (float tokens.Head))
            | "(" :: tail -> 
                let remTokens, value = Expression tail
                match remTokens with
                | ")" :: tail -> (tail, value)
                | _ -> raise (TokenMatchingError "Mismatched number of parentheses encountered.")
                
        snd (Expression accumulatedTokens)
        
        
    // let rec Execute (tokens : string list) (parenCount: int) (lastNumeric: bool) (lastParent : int) : bool =
    //     
    //     if tokens.IsEmpty then
    //         parenCount = 0
    //     else
    //         let Advance = Execute (tokens[1..])
    //         let currToken = tokens.Head
    //                    
    //         match currToken with
    //         | _ when not(IsDigit currToken || IsBinaryOperator currToken || currToken = "(" || currToken = ")") ->
    //             false
    //         | "(" when lastNumeric || lastParent = -1 -> // (Opening parenthesis not preceded by binOp)
    //             false                    
    //         | "(" ->
    //             Advance (parenCount + 1) false 1
    //         | ")" when parenCount <= 0 -> //Mismatching parenthesis
    //             false                    
    //         | ")" when lastParent = 1 -> //Empty parenthesis expression
    //             false                
    //         | ")" ->
    //             Advance (parenCount - 1) false -1
    //         | _ when IsBinaryOperator currToken && tokens.Length = 1 -> //Dangling binOp
    //             false
    //         | _ when IsBinaryOperator currToken && (lastNumeric || lastParent = -1) ->
    //             Advance parenCount false 0
    //         | _ when IsBinaryOperator currToken -> // Number not followed by binOp or parenthesis
    //             false
    //         | _ when IsDigit currToken && not(lastNumeric) ->
    //             Advance parenCount true 0
    //         | _ when IsDigit currToken -> // binOp not followed by number or parenthesis.
    //             false
    //         | _ ->
    //             false
    //             
    // Execute accumulatedTokens 0 false 0