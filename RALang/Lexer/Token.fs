module Transpiler.Lexer.Token

open System

type Token =
    | ADD | SUB | MUL | DIV | EXP
    | EQ | NEQ | GEQ | LEQ | GT | LT | AND | OR
    | ASSIGN | MUTATE | DECLARATION
    | L_PAR
    | R_PAR
    | I of int | F of float | B of bool | S of string
    | IDENTIFIER of string
    | TYPE of string
    | CONDITIONAL | IF | ELIF | THEN | ELSE | FI
    | LOOP | WHILE | DO | OD
    | BLOCK
    | MAIN
    | LINE_END
    
let BOpTokenMap = Map(seq {
    ("+", ADD); ("-", SUB); ("*", MUL); ("/", DIV); ("^", EXP);
})

let ROpTokenMap = Map(seq {
    ("!=", NEQ); ("==", EQ); (">=", GEQ); ("<=", LEQ); (">", GT); ("<", LT); ("||", OR); ("&", AND);
})

let ParenthesisTokenMap = Map(seq {("(", L_PAR); (")", R_PAR)})

let Types : Set<string> = Set(seq {"int"; "real"; "string"; "bool"})

let AssignTokenMap = Map(seq {("=", ASSIGN); (":=", MUTATE)})

let ConditionalTokenMap = Map(seq{("if", IF); ("elif", ELIF); ("then", THEN); ("else", ELSE); ("fi", FI)})

let LoopTokenMap = Map(seq{ ("while", WHILE); ("do", DO); ("od", OD) })

let TBoolean = Set(seq {"true"; "false"})

let UnaryOperator: Set<string> = Set(seq{ "-" ; "!" })
// let BinaryOperator : Set<string> = Set(seq {"+"; "-"; "*"; "/"; "^"})
// let RelationalOperator : Set<string> = Set(seq {"!="; "=="; ">="; "<="; ">"; "<"; "||"; "&&"})

let Parenthesis : Set<string> = Set(seq {"("; ")"})


let IsInteger (s: string) : bool =
     s.Length > 0 && (Seq.head s = '-' || Char.IsDigit (char(Seq.head s))) && Seq.forall Char.IsDigit s[1..]
     
let IsFloat (s: string) : bool =
    (Seq.head s = '-' || Char.IsDigit (char(Seq.head s))) &&
    Seq.forall (fun x ->  Char.IsDigit x || x = '.') s[1..] &&
    Seq.length (Seq.filter (fun x -> x = '.') s) = 1 &&
    Seq.last s <> '.' &&
    Seq.head s <> '.'

let isIdentifier (s: string) : bool =
    s.Length <> 0 &&
    Char.IsLetter(Seq.head s) && Char.IsLetter(Seq.last s) &&
    Seq.forall (fun x -> Char.IsLetterOrDigit x || x = '_') s[1.. s.Length - 1] 