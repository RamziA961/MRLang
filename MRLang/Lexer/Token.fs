
module MRLang.Token
open System.Collections.Generic

// let Keywords = Map<string, bool>[]

module Token =
    let Keywords : Set<string> =  Set(seq {"if"; "fi"})

    let Types : Set<string> = Set(seq {"int"; "float"; "string"; "bool"})

    let Operator : Set<char> = Set(seq {'+'; '-'; '*'; '/'; '^'; '('; ')'})