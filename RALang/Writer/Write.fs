module Transpiler.Writer.Write

open Transpiler.Lexer.TokenTranslation
open Transpiler.AbstractSyntaxTree.AbstractSyntaxTree
open Transpiler.Lexer.Token

///<summary> Assemble Node and Leafs into string.</summary>
let private AssembleOperation = fun (token : Token) -> fun (children: string list) ->
        (children[0..(children.Length-1) / 2] |> String.concat " ") +
        $" {KeywordMap.Item(token)} " +
        (children[(children.Length / 2)..] |> String.concat " ")
        
let private AssembleMonadic = fun (token : Token) -> fun (children: string list) ->
    match children with
    | "(" :: tail -> "( " + KeywordMap.Item(token) + (String.concat " " tail)
    | _ -> KeywordMap.Item(token) + (String.concat " " children)

let rec private DescendAndConcat(ast: AST) (depth: int): string =
        let out = [
            for child in ast.children do
                match ast.token with
                | BLOCK -> yield DescendAndConcat child (depth + 1)
                | _ -> yield DescendAndConcat child depth
        ]
        
        match ast.token with
        | PROGRAM ->
            String.concat "\n\n" out
        | PROC -> String.concat " " out
        | ARGS -> "(" + (String.concat ", " out) + ")"
        | BLOCK ->
            "{\n" + (
                [ for s in out do yield (String.replicate depth "\t") + s + ";\n" ]
                |> String.concat ""
            ) + (String.replicate (depth - 1) "\t") + "}"
        | CONDITIONAL -> String.concat "\n" out
        | IF -> "if (" + out[0] + ") " + out[1]
        | ELIF -> (String.replicate (depth - 1) "\t") + "else if (" + out[0] + ") " + out[1]
        | ELSE -> (String.replicate (depth - 1) "\t") + "else " + out[0]
        | LOOP -> String.concat "\n" out
        | WHILE -> "while (" + out[0] + ") " + out[1]
        | DECLARE -> out |> String.concat " "
        | ASSIGN -> "const " + AssembleOperation ASSIGN out
        | MUTATE -> AssembleOperation MUTATE out
        | TYPE t -> TypeMap.Item(t)
        | IDENTIFIER id -> id
        | ADD -> AssembleOperation ADD out
        | SUB -> AssembleOperation SUB out
        | MUL -> AssembleOperation MUL out
        | DIV -> AssembleOperation DIV out
        | EXP -> AssembleOperation EXP out
        | NOT -> AssembleMonadic NOT out
        | AND -> AssembleOperation AND out
        | OR -> AssembleOperation OR out
        | EQ -> AssembleOperation EQ out
        | NEQ -> AssembleOperation NEQ out
        | GT -> AssembleOperation GT out
        | GEQ -> AssembleOperation GEQ out
        | LT -> AssembleOperation LT out
        | LEQ -> AssembleOperation LEQ out
        | I v -> $"{v}"
        | F v -> $"{v}"
        | B v -> match v with | true -> "1" | false -> "0"
        | S v -> v
        | L_PAR -> "("
        | R_PAR -> ")"

let GenerateSourceCode(ast: AST) : string =
    DescendAndConcat ast 1