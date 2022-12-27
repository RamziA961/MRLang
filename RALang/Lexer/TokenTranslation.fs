module Transpiler.Lexer.TokenTranslation

open Token

let KeywordMap = Map(seq {
    (ADD, "+"); (SUB, "-"); (MUL, "*"); (DIV, "/"); (EXP, "^")
    (AND, "&&"); (OR, "||"); (EQ, "=="); (NEQ, "!="); (GT, ">"); (GEQ, ">="); (LT, "<"); (LEQ, "<="); (NOT, "!")
    (MAIN, "int main()")
    (ASSIGN, "="); (MUTATE, "=")
})

let TypeMap = Map(seq {
    ("int", "int"); ("real", "float"); ("char", "char*"); ("bool", "int")
})