module Parser

open FParsec
open Common

let firstIdentifier = "([_@a-zA-Z]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})"
let identifier = "([-_@a-zA-Z0-9]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})*"

let pIdentifier = regex (firstIdentifier + identifier)
let notSBrackets s = s <> '[' && s <> ']'
let pSBracketColumn = pstring "[" >>. many1Satisfy notSBrackets .>> pstring "]"
let pColumn = pIdentifier <|> pSBracketColumn
let pComma = spaces >>. pstring "," .>> spaces
let pColumnList = sepBy1 pColumn pComma

let pExpression, pExpressionRef = createParserForwardedToRef()

let pExprInExpr =
    pstring "(" >>. (pExpression <|> (pIdentifier |>> Identifier)) .>> pstring ")"

let pProjectExpression =
    pipe2 (pstring "project" >>. spaces >>. pExprInExpr .>> spaces) pColumnList
          (fun e c -> { Expression = e; ColumnList = c })

pExpressionRef.Value <-
    (pProjectExpression |>> Expression.ProjectExpression)
    <|> (pstring "(" >>. pIdentifier .>> pstring ")" |>> Identifier)

let pListStmt =
    pstring "list" >>% ListStmt

let pQuitStmt =
    pstring "quit" >>% QuitStmt

let pPrintStmt =
    pstring "print" >>. spaces >>. pIdentifier |>> PrintStmt

let pUseStmt =
    pstring "use" >>. spaces >>. pIdentifier |>> UseStmt

let pAssignStmt = 
    pipe2 (pIdentifier .>> spaces .>> pstring "=" .>> spaces) pExpression
          (fun r e -> { Rname = r; Expression = e })
    |>> AssignStmt

let pCommand: Parser<_, unit> = (pProjectExpression |>> ProjectExpression)
                                <|> pListStmt
                                <|> pQuitStmt
                                <|> pPrintStmt
                                <|> pUseStmt
                                <|> pAssignStmt
