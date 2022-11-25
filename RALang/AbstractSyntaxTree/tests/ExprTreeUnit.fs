module Transpiler.AbstractSyntaxTree.tests.ExprTreeUnit

open System
open Transpiler.AbstractSyntaxTree.ExprTree
open NUnit.Framework


let unmatchedParenthesisFalseCases = [
    [")"]
    ["("]
    ["("; "9"; "+"; "3"]
    [")"; "9"; "+"; "3"]
    ["9"; "+"; "3"; ")"]
    ["("; "9"; "+"; "3"; ")"; ")"]
    ["("; "3"; "+"; "("; "4"; "-"; "5"; "*"; "("; "2"; "/"; "2"; ")"]
    ["("; "9"; "+"; "3"; ")"; ")"]
    ["9"; "+"; "("; "3"; ")"; "+"; "2"; ")"]
    ["9"; "+"; ")"; "("; "3"; "+"; "2"; ")"]
    ["("; "("; "9"; "+"; "3"; ")"]
    ["9"; "+"; ")"; "("; "3"; "+"; "2"; "("]     
]

let unmatchedParenthesisTrueCases = []

//
// [<Test>]
// let emptyExpr() =
//     Assert.IsFalse(ExprTree.isExpr [])


// [<Test>]
// [TestCaseSourceAttribute(nameof(unmatchedParenthesisFalseCases))]
// [TestCaseSourceAttribute(nameof(unmatchedParenthesisTrueCases))]
// let unmatchedParenthesis(falseTests : string list list) (trueTests : string list list) =
//     for test in falseTests do
//         Assert.IsFalse(ExprTree.isExpr test)
//     for test in trueTests do
//         Assert.IsTrue(ExprTree.isExpr test)
//         
//
// [<Test>]
// let danglingOperators() =
//     Assert.IsFalse(ExprTree.isExpr ["+"])
//     Assert.IsFalse(ExprTree.isExpr ["+"; "9"; "+"; "3"])
//     Assert.IsFalse(ExprTree.isExpr ["("; "9"; "+"; "3"; "+"; ")"])
//     Assert.IsFalse(ExprTree.isExpr ["("; "9"; "+"; "3"; ")"; "+"])
//     Assert.IsFalse(ExprTree.isExpr ["("; "+"; "9"; "+"; "3"; ")"])
//
//
// let unsupportedElementOrders() =
//     Assert.IsFalse(ExprTree.isExpr ["9"; "+"; "*"; "6"])
//     Assert.IsFalse(ExprTree.isExpr ["9"; "+"; "-";])
//     Assert.IsFalse(ExprTree.isExpr ["9"; "8"])
//     Assert.IsFalse(ExprTree.isExpr ["9"; "8"; "7"])
//     Assert.IsFalse(ExprTree.isExpr ["9"; "8"; "+"; "7"])
//     Assert.IsFalse(ExprTree.isExpr ["9"; "+"; "8"; "7"])
//     Assert.IsFalse(ExprTree.isExpr ["2"; "/"; "/"; "9"; "+"; "8";])
//     Assert.IsFalse(ExprTree.isExpr ["8"; "("; "+"; ")"; "7"])
    