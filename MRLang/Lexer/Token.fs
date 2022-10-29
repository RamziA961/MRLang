
module MRLang.Token
open System.Collections.Generic

// let Keywords = Map<string, bool>[]

module Token =
    let Keywords : Set<string> =  Set(seq {"if"; "fi"})
    let Types : Set<string> = Set(seq {"int"; "float"; "string"; "bool"})
    
    let TBoolean = Set(seq {"T"; "F"})
    
    let BinaryOperator : Set<string> = Set(seq {"+"; "-"; "*"; "/"; "^"})
    let RelationalOperator : Set<string> = Set(seq {"!="; "=="; ">="; "<="; ">"; "<"; "||"; "&&"})

    let Parenthesis : Set<string> = Set(seq {"("; ")"})
    
    
    let Tokens = Set.unionMany(seq {Keywords; Types; BinaryOperator; RelationalOperator})