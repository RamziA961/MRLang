module Transpiler.Lexer.Lexer

open System
open System.Text.RegularExpressions
open Token

exception LexingError of string
exception UnsupportedKeywordError of string


let private (|Digit|_|) (c : char) =
    if List.contains c ['0'..'9'] then Some(c) else None
        
let private (|DigitNoZero|_|) (c : char) =
    if List.contains c ['0'..'9'] then Some(c) else None
    
let private (|Letter|_|) (c: char) =
    if Char.IsLetter c then Some(c) else None

let private (|RelOp|_|) (c: char) =
    if List.contains c ['='; '!'; '<'; '>'] then Some(c) else None

let private (|LogOp|_|) (c: char) =
    if List.contains c ['&'; '|'] then Some(c) else None
    
let private (|AssignOp|_|) (c: char) =
    if List.contains c ['='; ':'] then Some(c) else None

let private Filter input: string =
    //Source of Regular Expression:
    //Finding Comments in Source Code Using Regular Expressions by Stephen Ostermiller
    //https://blog.ostermiller.org/finding-comments-in-source-code-using-regular-expressions/
    Regex.Replace(input, "/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/", "")    
            
        
// let private Tokenize (inputString : string) : Token list =
//     let MatchToken (input : string) (accumulated : Token list): Token =
//         match input with
//         | "true" | "false" -> B (input = "true")
//         | _ when input.StartsWith "\"" && input.EndsWith "\"" -> S input
//         | _ when BOpTokenMap.ContainsKey input ->  BOpTokenMap.Item input
//         | _ when ROpTokenMap.ContainsKey input -> ROpTokenMap.Item input
//         | _ when ConditionalTokenMap.ContainsKey input -> ConditionalTokenMap.Item input
//         | _ when LoopTokenMap.ContainsKey input -> LoopTokenMap.Item input
//         | _ when IsFloat input ->
//             F (float input)
//         | _ when IsInteger input -> I (int input)
//         | "(" | ")" -> ParenthesisTokenMap.Item input
//         | "\n" | ";" -> LINE_END
//         | _ when Types.Contains input -> TYPE input
//         | _ when isIdentifier input -> IDENTIFIER input
//         | "=" -> ASSIGN
//         | ":=" -> MUTATE
//         | _ ->
//             raise( UnsupportedKeywordError $"Lexing Error: '%s{input}' is not a supported keyword." )
//                 
//     let rec Scan (input: string) (accumulator : Token list) (cursor : int) (canSplit : bool) : Token list =
//         if cursor >= input.Length then
//             if input = "" then accumulator @ [LINE_END] else accumulator @ [MatchToken input accumulator; LINE_END]
//         else
//             let SliceAndScan = Scan (input.Substring(cursor + 1))
//             let Advance = Scan input accumulator (cursor + 1)
//                
//             match input[cursor] with
//             | '"' when canSplit ->
//                 // prevent splitting when first double quote found
//                 Advance false
//                     
//             | '"' when not canSplit ->
//                 // allow splitting when second double quote found
//                 Advance true
//             | ' ' when input[0 .. cursor - 1] <> "" && canSplit ->
//                 // (space or newline character found) & (buffer not empty) & (not reading a string value)
//                 SliceAndScan (accumulator @ [MatchToken(input[0 .. cursor - 1]) accumulator]) 0 canSplit
//             | '\n' | ';' when input[0 .. cursor - 1] <> "" && canSplit ->
//                 SliceAndScan (
//                     accumulator @
//                     [MatchToken(input[0 .. cursor - 1]) accumulator] @
//                     [MatchToken($"{input[cursor]}") accumulator]
//                 ) 0 canSplit
//             | '\n' | ';' when canSplit ->
//                 SliceAndScan (
//                     accumulator @
//                     [MatchToken($"{input[cursor]}") accumulator]
//                 ) 0 canSplit
//             | ' ' | '\n' | ';' when canSplit ->
//                 // SliceAndScan (
//                 //     accumulator @
//                 //     [MatchToken($"{input[cursor]}") accumulator]
//                 // ) 0 canSplit
//                 Scan input[1..] accumulator cursor canSplit
//             | ' ' | '\t' when canSplit ->  
//                 // filter out spaces between tokens
//                 SliceAndScan accumulator 0 canSplit
//             | '-' when canSplit ->
//                 if cursor >= 1 then
//                     Scan (input.Substring cursor) (accumulator @ [MatchToken input[0.. cursor-1] accumulator]) 1 canSplit 
//                 else
//                     Advance canSplit
//             | _ when BOpTokenMap.ContainsKey($"{input[cursor]}")
//                 || ROpTokenMap.ContainsKey($"{input[cursor]}")
//                 || ParenthesisTokenMap.ContainsKey($"{input[cursor]}") ->
//                 if cursor >= 1 then
//                     SliceAndScan
//                         (accumulator @
//                             [MatchToken(input[0 .. cursor-1]) accumulator; MatchToken(input[cursor..cursor]) accumulator]
//                         )
//                         0 canSplit
//                 else
//                     SliceAndScan (accumulator @ [MatchToken(input[0 .. cursor]) accumulator]) 0 canSplit
//             | _ ->
//                 Advance canSplit
//     Scan inputString [] 0 true              

let rec Tokenize (input: char list, accumulated: Token list) =
    match input with
    | [] -> [], accumulated
    | (' ' | '\t') :: tail -> Tokenize (tail, accumulated) 
    | ('\n' | ';') :: tail -> Tokenize (tail, (accumulated @ [LINE_END]))
    | _ :: _ ->
        let Pipeline = (Number >> Boolean >> Str >> Parenthetical >> BinaryOperator >> RelationalOperator >>
            LogicalOperator >> TypeDefinition >> Assignment >> Keyword >> Identifier) 
       
        let rem, tokens : char list * Token list = Pipeline (input, accumulated)
        if tokens.Length = rem.Length then raise(
            UnsupportedKeywordError $"Unsupported keyword encountered. Lexing Halted\nBUFFER DUMP: %A{input}."
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
        | DigitNoZero _ :: tail->
           let rem, intStr =  Collect tail ""
           match rem with
           | '.' :: tail ->
               let rem, floatStr = Collect tail ""
               rem, accumulated @ [F (float $"-{intStr}.{floatStr}")]
           | _ -> rem, accumulated @ [I (int $"-{intStr}")]
        | _ -> input, accumulated
    | DigitNoZero _ :: _ ->
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
        | Letter l :: tail -> Collect tail (accumulated + (string l))
        | _ -> input, accumulated
    let rem, str = Collect input ""
    match str with
    | "true" | "false" -> rem, accumulated @ [B (str = "true")]
    | _ -> input, accumulated
and Str (input: char list, accumulated: Token list) =
    let rec Collect (input: char list) (accumulated : string) =
        match input with
        | '"' :: tail -> tail, accumulated + (string input[0])
        | _ :: tail -> tail, accumulated
        | _ -> raise (
                LexingError (
                    "Lexing error encountered. Instance of unterminated string." +
                    $"Accumulated String Dump: {accumulated}.\n" +
                    $"Remaining Character Dump: {input}."
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
    | "!=" -> rem, accumulated @ [NEQ]
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
and TypeDefinition (input: char list, accumulated: Token list) =
    let rec Collect (input: char list) (accumulated: string) =
        match input with
        | Letter l :: tail -> Collect tail (accumulated + string l)
        | _ -> input, accumulated
    
    let rem, str = Collect input ""
    match str with
    | "int" -> rem, accumulated @ [TYPE "int"]
    | "real" -> rem, accumulated @ [TYPE "real"]
    | "string" -> rem, accumulated @ [TYPE "string"]
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
    | _ -> input, accumulated
and Identifier (input: char list, accumulated: Token list) =
    let rec Collect (input: char list) (accumulated: string) =
        match input with
        | Letter l :: tail -> tail, (accumulated + string l)
        | Digit d :: tail -> tail, (accumulated + string d)
        | '_' :: tail -> tail, (accumulated + "_")
        | _ -> input, accumulated
    
    match input with
    | Letter _ :: _ ->
        let rem, str = Collect input ""
        rem, accumulated @ [IDENTIFIER str]
    | '_' :: _ ->
        let rem, str = Collect input ""
        rem, accumulated @ [IDENTIFIER str]
    | _ -> input, accumulated
        
        
let Lex (input: string) : Token list =
    snd (Tokenize (input.Trim [|' '|] |> Filter |> Seq.toList, []))