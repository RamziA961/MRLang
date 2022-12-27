module Transpiler.Lexer.Token

open System

type Token =
    | I of int | F of float | B of bool | S of string
    | IDENTIFIER of string
    | TYPE of string
    | ADD | SUB | MUL | DIV | EXP
    | EQ | NEQ | GEQ | LEQ | GT | LT | AND | OR | NOT
    | ASSIGN | MUTATE | DECLARE
    | L_PAR | R_PAR
    | CONDITIONAL | IF | ELIF | THEN | ELSE | FI
    | LOOP | WHILE | DO | OD
    | BLOCK
    | PROC | ARGS | BEGIN | END | PROC_CALL
    | PROGRAM
    | COLON | SEP | LINE_END

let ReservedKeyword = Set(seq {
    "not"
    "int"; "float"; "real"; "string"; "bool"; "char"
    "if"; "fi"; "else"; "elif"; "then"
    "while"; "do"; "od"
    "true"; "false"
    "proc"; "begin"; "end"
})

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