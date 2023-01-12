module Transpiler.Lexer.Token

/// Valid tokens for use during transpilation process. Some tokens are generated during parsing.
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

/// Reserved keywords that cannot be used as identifier names. 
let ReservedKeyword = Set(seq {
    "not"
    "int"; "float"; "real"; "string"; "bool"; "char"
    "if"; "fi"; "else"; "elif"; "then"
    "while"; "do"; "od"
    "true"; "false"
    "proc"; "begin"; "end"
})