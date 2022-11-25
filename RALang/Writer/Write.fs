module Transpiler.Writer.Write

open Transpiler.Lexer.TokenTranslation
open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.Lexer.Token

///<summary> Assemble Node and Leafs into string.</summary>
///<code>(Left Children, Parent, Right Children)</code>
///<code>
///        (+)           -> 1 + 3 + 2 - 5
///  1 + 3     2 - 5  
///</code>
let private AssembleOperation = fun (token : Token) -> fun (children: string list) ->
        (children[0..(children.Length-1) / 2] |> String.concat " ") +
        $" {KeywordMap.Item(token)} " +
        (children[(children.Length / 2)..] |> String.concat " ")

let rec private DescendAndConcat(ast: AST) : string =
        let out = [for child in ast.children do yield DescendAndConcat child] 
        match ast.token with
        | MAIN ->
            [(KeywordMap.Item MAIN); (out |> String.concat "")] |> String.concat ""
        | BLOCK ->
            "{\n" + ([for s in out do yield s + ";\n"] |> String.concat "") + "}"
        | DECLARATION -> out |> String.concat " "
        | ASSIGN -> AssembleOperation ASSIGN out
        | MUTATE -> AssembleOperation MUTATE out
        | TYPE t -> TypeMap.Item(t)
        | IDENTIFIER id -> id
        | ADD -> AssembleOperation ADD out
        | SUB -> AssembleOperation SUB out
        | MUL -> AssembleOperation MUL out
        | DIV -> AssembleOperation DIV out
        | EXP -> AssembleOperation EXP out
        | AND -> AssembleOperation ADD out
        | OR -> AssembleOperation OR out
        | EQ -> AssembleOperation EQ out
        | NEQ -> AssembleOperation NEQ out
        | I v -> $"{v}"
        | F v -> $"{v}"
        | B v -> match v with | true -> "1" | false -> "0"
        | S v -> v
        | L_PAR -> "("
        | R_PAR -> ")"

let GenerateSourceCode(ast: AST) : string =
    DescendAndConcat ast