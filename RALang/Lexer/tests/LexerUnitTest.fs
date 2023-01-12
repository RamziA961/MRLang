module Transpiler.Lexer.tests.LexerUnitTest

open NUnit.Framework

open Transpiler.Lexer.Token
open Transpiler.Lexer.Lexer


[<Test>]
[<Category("Lexer")>]
[<Parallelizable(ParallelScope.Self)>]
let NumericalTests () =
    let expectedEqualCases = [
        ("321", [I 321])
        ("-321", [I -321])
        ("321.0", [F 321.0])
        ("-321.0", [F -321.0])
        ("0", [I 0])
        ("-0", [I 0])
        ("321 123", [I 321; I 123])
        ("321.0321 123", [F 321.0321; I 123])
        ("-321.0321 -123", [F -321.0321; I -123])
        ("0.342", [F 0.342])
        ("-0.342", [F -0.342])
        ("0 0 0.342", [I 0; I 0; F 0.342])
        ("12345678 0 0.342", [I 12345678; I 0; F 0.342])
    ]
    
    let expectFailCases = [
        ".0332"
        "-.0321"
        "0.0.23"
        "0..023"
        "0.-932"
        "-.021"
        "0."
        "12."
    ]
    
    for case, expected in expectedEqualCases do        
        Assert.AreEqual(Lex case, expected)
        
    for case in expectFailCases do
        Assert.Throws<UnsupportedKeywordError>(fun () -> Lex case |> ignore) |> ignore
    
    Assert.Pass()
        
[<Test>]
[<Category("Lexer")>]
[<Parallelizable(ParallelScope.Self)>]
let BooleanTest () =
    let expectedEqualCases = [
        ("false", [B false])
        ("true", [B true])
        ("true false true", [B true; B false; B true])
    ]
    
    for case, expected in expectedEqualCases do
        Assert.AreEqual(Lex case, expected)
        
[<Test>]
[<Category("Lexer")>]
[<Parallelizable(ParallelScope.Self)>]
let StringTest () =
    let expectedEqualCases = [
        ("\"Hello World\"", [S "\"Hello World\""])
        ("\"Hello\nWorld\"", [S "\"Hello\nWorld\""])
        ("\"while\"", [S "\"while\""])
        ("\"int\"", [S "\"int\""])
    ]
    
    let expectedFailCases = [
       "\"int"
       "int\""
    ]
    
    for case, expected in expectedEqualCases do
        let out = Lex case
        Assert.AreEqual(out, expected)
        
    for case in expectedFailCases do
        Assert.Throws<LexingError>(fun () -> Lex case |> ignore) |> ignore
        
    Assert.Pass()
        
[<Test>]
[<Category("Lexer")>]
[<Parallelizable(ParallelScope.Self)>]
let KeywordTest () =
    let expectedEqualCases = [
        ("while while", [WHILE; WHILE])
        ("begin", [BEGIN])
        ("end", [END])
        ("do", [DO])
        ("od", [OD])
        ("proc", [PROC])
        ("if", [IF])
        ("then", [THEN])
        ("elif", [ELIF])
        ("else", [ELSE])
        ("fi", [FI])
    ]
    
    for case, expected in expectedEqualCases do
        Assert.AreEqual(Lex case, expected)
        
[<Test>]
[<Category("Lexer")>]
[<Parallelizable(ParallelScope.Self)>]
let OperatorTest () =
    let expectedEqualCases = [
        ("=", [ASSIGN])
        (":=", [MUTATE])
        ("+", [ADD])
        ("-", [SUB])
        ("*", [MUL])
        ("/", [DIV])
        ("^", [EXP])
        ("==", [EQ])
        ("~=", [NEQ])
        (">", [GT])
        (">=", [GEQ])
        ("<", [LT])
        ("<=", [LEQ])
        ("&", [AND])
        ("|", [OR])
        ("not ", [NOT])
    ]
    
    for case, expected in expectedEqualCases do
        let out = Lex case
        Assert.AreEqual(out, expected)


[<Test>]
[<Category("Lexer")>]
[<Parallelizable(ParallelScope.Self)>]
let TypeTest () =
    let expectedEqualCases = [
        ("int", [TYPE "int"])
        ("real", [TYPE "real"])
        ("bool", [TYPE "bool"])
        ("char", [TYPE "char"])
    ]
    
    for case, expected in expectedEqualCases do
        Assert.AreEqual(Lex case, expected)

[<Test>]
[<Category("Lexer")>]
[<Parallelizable(ParallelScope.Self)>]
let IdentifierTest () =
    let expectedEqualCases = [
        ("a", [IDENTIFIER "a"])
        ("a_c", [IDENTIFIER "a_c"])
        ("a_cd", [IDENTIFIER "a_cd"])
        ("da_c", [IDENTIFIER "da_c"])
        ("acd", [IDENTIFIER "acd"])
        ("a0", [IDENTIFIER "a0"])
        ("a_0", [IDENTIFIER "a_0"])
        ("_a_0", [IDENTIFIER "_a_0"])
    ]
    
    let expectedFailCases = [
        "0acd"
        "_"
    ]
    
    for case, expected in expectedEqualCases do
        Assert.AreEqual(Lex case, expected)
        
    for case in expectedFailCases do
        Assert.Throws<UnsupportedKeywordError>(fun () -> Lex case |> ignore) |> ignore
    
    Assert.Pass()