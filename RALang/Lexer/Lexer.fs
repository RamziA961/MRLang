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

let private Filter input: string =
    //Source of Regular Expression:
    //Finding Comments in Source Code Using Regular Expressions by Stephen Ostermiller
    //https://blog.ostermiller.org/finding-comments-in-source-code-using-regular-expressions/
    Regex.Replace(input, "/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/", "")    
           
           
let rec private Tokenize (input: char list, accumulated: Token list) =
    match input with
    | [] -> [], accumulated
    | (' ' | '\t') :: tail -> Tokenize (tail, accumulated) 
    | ('\n' | ';') :: tail -> Tokenize (tail, (accumulated @ [LINE_END]))
    | ',' :: tail -> Tokenize (tail, (accumulated @ [SEP]))
    | ':' :: tail when tail[0] <> '=' -> Tokenize (tail, (accumulated @ [COLON]))
    | _ :: _ ->
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
            
        if input.Length = rem.Length then raise(
            UnsupportedKeywordError $"Unsupported keyword encountered. Lexing Halted.\nBUFFER DUMP: %A{input}."
        ) else Tokenize (rem, tokens)
and Number (input: char list, accumulated: Token list) =
    let rec Collect (input: char list) (accumulated : string) =
        match input with
        | Digit c :: tail -> 
            Collect tail (accumulated + (string c)) 
        | _ -> input, accumulated
    
    match input with
    | '-' :: tail ->
        match tail with
        | Digit _ :: tail->
           let rem, intStr =  Collect tail ""
           match rem with
           | '.' :: tail ->
               let rem, floatStr = Collect tail ""
               rem, accumulated @ [F (float $"-{intStr}.{floatStr}")]
           | _ -> rem, accumulated @ [I (int $"-{intStr}")]
        | _ -> input, accumulated
    | Digit _ :: _ ->
        let rem, intStr =  Collect input ""
        match rem with
        | '.' :: tail ->
           let rem, floatStr = Collect tail ""
           rem, accumulated @ [F (float $"{intStr}.{floatStr}")]
        | _ -> rem, accumulated @ [I (int $"{intStr}")]
    | _ -> input, accumulated
and Boolean (input: char list, accumulated: Token list) =
    let rec Collect (input: char list) (accumulated: string) =
        match input with
        | Letter l :: tail -> Collect tail (accumulated + string l)
        | _ -> input, accumulated
    let rem, str = Collect input ""
    match str with
    | "true" | "false" -> rem, accumulated @ [B (str = "true")]
    | _ -> input, accumulated
and Str (input: char list, accumulated: Token list) =
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
        rem, accumulated @ [S ("\"" + str)] 
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
    let rec Collect (input: char list) (accumulated: string) =
        match input with
        | LogOp l :: tail ->  Collect tail (accumulated + string l)  
        | _ -> input, accumulated
        
    let rem, str = Collect input ""
    match str with
    | "&" -> rem, accumulated @ [AND]
    | "||" -> rem, accumulated @ [OR]
    | _ -> rem, accumulated
and MonadicOperator (input: char list, accumulated: Token list) =
    match input with
    | 'n' :: 'o' :: 't' :: ' ' :: tail -> tail, accumulated @ [NOT]
    | _ -> input, accumulated
and TypeDefinition (input: char list, accumulated: Token list) =
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
        if ReservedKeyword.Contains str then input, accumulated else rem, accumulated @ [IDENTIFIER str]
    | _ -> input, accumulated
        
         
let Lex (input: string) : Token list =
    snd (Tokenize (input.Trim [|' '|] |> Filter |> Seq.toList, []))