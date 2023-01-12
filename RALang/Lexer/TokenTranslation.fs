module Transpiler.Lexer.TokenTranslation

open Token

/// Algol to C keyword map.
let KeywordMap = Map(seq {
    (ADD, "+"); (SUB, "-"); (MUL, "*"); (DIV, "/"); (EXP, "^")
    (AND, "&&"); (OR, "||"); (EQ, "=="); (NEQ, "!="); (GT, ">"); (GEQ, ">="); (LT, "<"); (LEQ, "<="); (NOT, "!")
    (ASSIGN, "="); (MUTATE, "=")
})

// Algol to C type map.
let TypeMap = Map(seq {
    ("int", "int"); ("real", "float"); ("char", "char*"); ("bool", "int")
})