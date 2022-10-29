namespace MRLang.AbstractSyntaxTree

type AST =
        {
            children : List<AST>
            decoration : string
            token : string
        }

        member this.AppendChild (child: AST) : AST =
            { this with children = this.children @ [child] }        
        