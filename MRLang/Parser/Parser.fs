namespace Parser

open System
open MRLang.Token
open MRLang.AbstractSyntaxTree

module Parser = 
    exception UnsupportedKeywordError of string
    exception TokenMatchingError of string

    let private Expect (tokenList: List<string>) (token : string) =
        let rec Search (index : int) : bool =
            if index >= tokenList.Length then
                false
            else
                match tokenList[index] with
                | "\n" -> false
                | _ when token = tokenList[index] -> true
                | _ -> Search (index + 1) 
       
        if not(Search 0) then
            raise (TokenMatchingError $"Parsing error encountered. Expected {token}.")
        else
            ()
    
    // let Execute (ast : AST) (tokens : string list) : AST =
    //     let ExpectToken = Expect tokens
        
        // let rec Parse (ast : AST) (tokens : string list) (accumulator : string list) : AST =
        //     
        //     match ast.GetToken() with
        //     | "\n" ->
        //         (*
        //            
        //         *)
        //         ()
        //     | "(" ->
        //         ExpectToken ")"
        //         Parse (ast.AppendChild(new AST([], "", ""))) tokens[1 .. tokens.Length] accumulator
        //    
        //     | _ ->
        //         raise(UnsupportedKeywordError $"Unsupported keyword detected: { ast.GetToken }")
        
        (*let rec construct (parent: AST) (accumulator: string list) =
            
            if not(parent) then
                let fstToken = accumulator.Head
                construct (new AST([], "", fstToken))
            else
               parent.AppendChild()*)
               