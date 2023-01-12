module Transpiler.Lexer.Lexer

open System
open System.Text.RegularExpressions
open Token

exception LexingError of string
exception UnsupportedKeywordError of string

let private (|Digit|_|) (c : char) = if List.contains c ['0'..'9'] then Some(c) else None
let private (|NonZeroDigit|_|) (c : char) = if List.contains c ['1'..'9'] then Some(c) else None
let private (|Letter|_|) (c: char) = if Char.IsLetter c then Some(c) else None
let private (|RelOp|_|) (c: char) = if List.contains c ['='; '~'; '<'; '>'] then Some(c) else None
let private (|LogOp|_|) (c: char) = if List.contains c ['&'; '|'] then Some(c) else None
let private (|AssignOp|_|) (c: char) = if List.contains c ['='; ':'] then Some(c) else None

/// <summary>
/// A function that converts source code into tokens. It traverses a list of characters
/// matching a single character or sequences of characters with an appropriate token.
/// </summary>
/// <param name="input">Source code converted to a character list to tokenize.</param>
/// <param name="accumulated">An empty list that stores accumulated tokens.</param>
/// <returns>
/// A tuple containing an empty list where source code was extracted from and a list of
/// tokens.
/// </returns>
/// <exception cref="UnsupportedKeywordError">
/// Thrown when the Tokenizer cannot advance due to an unrecognized token.
/// </exception>
/// <exception cref="LexingError">
/// Thrown when the Tokenizer encounters unterminated strings.
/// </exception>
let rec private Tokenize (input: char list, accumulated: Token list): char list * Token list =
    match input with
    | [] -> [], accumulated
    | (' ' | '\t') :: tail -> Tokenize (tail, accumulated) 
    | ('\n' | ';') :: tail -> Tokenize (tail, (accumulated @ [LINE_END]))
    | ',' :: tail -> Tokenize (tail, (accumulated @ [SEP]))
    | ':' :: tail when tail[0] <> '=' -> Tokenize (tail, (accumulated @ [COLON]))
    | _ :: _ ->
        // Lexing pipeline.
        let rem, tokens : char list * Token list =
            (Number >>
            Boolean >>
            Str >>
            Parenthetical >>
            BinaryOperator >>
            RelationalOperator >>
            LogicalOperator >>
            MonadicOperator >>
            TypeDefinition >>
            Assignment >>
            Keyword >>
            Identifier) (input, accumulated)
            
        // Forward progress was not made after full traversal of pipeline.
        if input.Length = rem.Length then raise(
            UnsupportedKeywordError $"Unsupported keyword encountered. Lexing Halted.\nBUFFER DUMP: %s{string input}."
        ) else Tokenize (rem, tokens)
and Number (input: char list, accumulated: Token list) =
    // collect digit characters
    let rec Collect (input: char list) (accumulated : string) =
        match input with
        | Digit c :: tail -> 
            Collect tail (accumulated + (string c))
        | _ -> input, accumulated
    
    let FormNumeric (input: char list) (accumulated : string) : char list * Token option =
        let rem, intStr =  Collect input accumulated
        
        if intStr.Length = accumulated.Length then
            input, None
        else
            match rem with
            | '.' :: tail -> // handle floating points
                // collect digits following decimal point
                let rem, floatStr = Collect tail ""

                match rem, floatStr with
                | Letter _ :: _, _ ->
                    // numerical values should not be directly followed by a letter
                    input, None 
                | _ when floatStr.Length > 0 && floatStr |> Seq.forall Char.IsDigit ->
                    rem, Some (F (float $"{intStr}.{floatStr}")) // append float token
                | _ -> input, None
            | Letter _ :: _ ->
                // numerical values should not be directly followed by a letter
                input, None   
            | _ -> rem, Some (I (int $"{intStr}")) // append integer token
    
    match input with
    | '-' :: tail -> // handle negative numbers
        match FormNumeric tail "-" with
        | rem, Some(tok) -> rem, accumulated @ [tok]
        | _ -> input, accumulated
    | Digit _ :: _ -> // handle positive numbers or zero
        match FormNumeric input "" with
        | rem, Some(tok) -> rem, accumulated @ [tok]
        | _ -> input, accumulated
    | _ -> input, accumulated
  
and Boolean (input: char list, accumulated: Token list) =
    // collect characters to form word
    let rec Collect (input: char list) (accumulated: string) =
        match input with
        | Letter l :: tail -> Collect tail (accumulated + string l)
        | _ -> input, accumulated
    let rem, str = Collect input ""
    match str with
    | "true" | "false" -> rem, accumulated @ [B (str = "true")] // append boolean token
    | _ -> input, accumulated
and Str (input: char list, accumulated: Token list) =
    // collect string literal
    let rec Collect (input: char list) (accumulated : string) =
        match input with
        | '"' :: tail -> tail, accumulated + string input[0]
        | _ :: tail -> Collect tail (accumulated + string input[0])
        | _ -> raise (
                LexingError (
                    "Lexing error encountered. Instance of unterminated string." +
                    $"TOKEN DUMP: {accumulated}.\n" +
                    $"BUFFER DUMP: {input}."
                )
            ) 
    match input with
    | '"' :: tail ->
        let rem, str = Collect tail ""
        rem, accumulated @ [S ("\"" + str)] // append string token
    | _ -> input, accumulated
and Parenthetical (input: char list, accumulated: Token list) =
    match input with
    | '(' :: tail -> tail, accumulated @ [L_PAR]
    | ')' :: tail -> tail, accumulated @ [R_PAR]
    | _ -> input, accumulated
and BinaryOperator (input: char list, accumulated: Token list) =
    match input with
    | '+' :: tail -> tail, accumulated @ [ADD]
    | '-' :: tail -> tail, accumulated @ [SUB]
    | '*' :: tail -> tail, accumulated @ [MUL]
    | '/' :: tail -> tail, accumulated @ [DIV]
    | '^' :: tail -> tail, accumulated @ [EXP]
    | _ -> input, accumulated
and RelationalOperator (input: char list, accumulated: Token list) =
    // collect valid relational operator characters into a single string 
    let rec Collect (input: char list) (accumulated: string) =
        match input with
        | RelOp r :: tail ->  Collect tail (accumulated + string r)  
        | _ -> input, accumulated   
    let rem, str = Collect input ""
    match str with
    | "==" -> rem, accumulated @ [EQ]
    | "~=" -> rem, accumulated @ [NEQ]
    | "<" -> rem, accumulated @ [LT]
    | ">" -> rem, accumulated @ [GT]
    | "<=" -> rem, accumulated @ [LEQ]
    | ">=" -> rem, accumulated @ [GEQ]
    | _ -> input, accumulated
and LogicalOperator (input: char list, accumulated: Token list) =
    // collect valid logical operator characters into a single string 
    let rec Collect (input: char list) (accumulated: string) =
        match input with
        | LogOp l :: tail ->  Collect tail (accumulated + string l)  
        | _ -> input, accumulated
        
    let rem, str = Collect input ""
    match str with
    | "&" -> rem, accumulated @ [AND]
    | "|" -> rem, accumulated @ [OR]
    | _ -> rem, accumulated
and MonadicOperator (input: char list, accumulated: Token list) =
    // handle negation
    match input with
    | 'n' :: 'o' :: 't' :: ' ' :: tail -> tail, accumulated @ [NOT]
    | _ -> input, accumulated
and TypeDefinition (input: char list, accumulated: Token list) =
    // collect characters to form word
    let rec Collect (input: char list) (accumulated: string) =
        match input with
        | Letter l :: tail -> Collect tail (accumulated + string l)
        | _ -> input, accumulated
    
    let rem, str = Collect input ""
    match str with
    | "int" -> rem, accumulated @ [TYPE "int"]
    | "real" -> rem, accumulated @ [TYPE "real"]
    | "char" -> rem, accumulated @ [TYPE "char"]
    | "bool" -> rem, accumulated @ [TYPE "bool"]
    | _ -> input, accumulated
and Assignment (input: char list, accumulated : Token list) =
    // collect valid assigment operators into a single string
    let rec Collect (input: char list) (accumulated: string) =
        match input with
        | AssignOp a :: tail -> Collect tail (accumulated + string a)
        | _ -> input, accumulated
        
    let rem, str = Collect input ""
    match str with
    | "=" -> rem, accumulated @ [ASSIGN]
    | ":=" -> rem, accumulated @ [MUTATE]
    | _ -> input, accumulated
and Keyword (input: char list, accumulated: Token list) =
    // collect characters to form word
    let rec Collect (input: char list) (accumulated: string) =
        match input with
        | Letter l :: tail -> Collect tail (accumulated + (string l))
        | _ -> input, accumulated
     
    let rem, str = Collect input ""
    match str with
    | "if" -> rem, accumulated @ [IF]
    | "then" -> rem, accumulated @ [THEN]
    | "elif" -> rem, accumulated @ [ELIF]
    | "else" -> rem, accumulated @ [ELSE]
    | "fi" -> rem, accumulated @ [FI]
    | "while" -> rem, accumulated @ [WHILE]
    | "do" -> rem, accumulated @ [DO]
    | "od" -> rem, accumulated @ [OD]
    | "proc" -> rem, accumulated @ [PROC]
    | "begin" -> rem, accumulated @ [BEGIN]
    | "end" -> rem, accumulated @ [END]
    | _ -> input, accumulated
and Identifier (input: char list, accumulated: Token list) =
    // collect valid character sequences to form word
    let rec Collect (input: char list) (accumulated: string) =
        match input with
        | Letter l :: tail -> Collect tail (accumulated + string l)
            | Digit d :: tail -> Collect tail (accumulated + string d)
        | '_' :: tail -> Collect tail (accumulated + "_")
        | _ -> input, accumulated
    
    match input with
    | Letter _ :: _ ->
        let rem, str = Collect input ""
        if ReservedKeyword.Contains str then input, accumulated else rem, accumulated @ [IDENTIFIER str]
    | '_' :: _ ->
        let rem, str = Collect input ""
        if ReservedKeyword.Contains str || input.Length = 1 then
            input, accumulated
        else rem, accumulated @ [IDENTIFIER str]
    | _ -> input, accumulated
        

/// <summary>
///  A lexer that tokenizes Algol source code into tokens.
/// </summary>
/// <param name="input">A source code string.</param>
/// <returns>A token list derived from the source code.</returns>
let Lex (input: string) : Token list =    
    snd ((input |> Seq.toList, []) |> Tokenize)