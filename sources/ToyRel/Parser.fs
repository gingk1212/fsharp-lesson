module Parser

open FParsec
open Common

let firstIdentifier = "([_@a-zA-Z]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})"
let identifier = "([-_@a-zA-Z0-9]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})*"

let pIdentifier =
    regex (firstIdentifier + identifier)
    |>> Identifier.Identifier

let notSBrackets s = s <> '[' && s <> ']'
let pSBracketColumn =
    pstring "[" >>. many1Satisfy notSBrackets .>> pstring "]"
    |>> SBracketColumn

let pColumn =
    (pIdentifier |>> Column.Identifier)
    <|> pSBracketColumn

let pComma = spaces >>. pstring "," .>> spaces
let pColumnList = sepBy1 pColumn pComma

let pExpression, pExpressionRef = createParserForwardedToRef()

let pExprInExpr =
    pstring "(" >>. (pExpression <|> (pIdentifier |>> Expression.Identifier)) .>> pstring ")"

let pProjectExpression =
    pipe2 (pstring "project" >>. spaces >>. pExprInExpr .>> spaces) pColumnList
          (fun e c -> { Expression = e; ColumnList = c })

let pDifferenceExpression =
    pipe2 pExprInExpr (spaces >>. pstring "difference" >>. spaces >>. pExprInExpr)
          (fun e1 e2 -> { Expression1 = e1; Expression2 = e2 })

// Since it is difficult to distinguish whether the right-hand sides of the
// following statements are Identifier or DifferenceExpression, apply attempt
// to pIdentifier.
// > hoge = (Employee)
// > hoge = (project (Employee) DeptName) difference (project (Dept) DeptName)
pExpressionRef.Value <-
    attempt(pstring "(" >>. pIdentifier .>> pstring ")" |>> Expression.Identifier)
    <|> (pProjectExpression |>> Expression.ProjectExpression)
    <|> (pDifferenceExpression |>> Expression.DifferenceExpression)

let pListStmt =
    pstring "list" >>% ListStmt

let pQuitStmt =
    pstring "quit" >>% QuitStmt

let pPrintStmt =
    pstring "print" >>. spaces >>. pIdentifier |>> PrintStmt

let pUseStmt =
    pstring "use" >>. spaces >>. pIdentifier |>> UseStmt

// Since pAssignStmt is at the top, the following statements are mismatched. So
// apply attempt.
// > print Employee
// > project (Employee) Name, EmpId, DeptName
let pAssignStmt =
    attempt (pipe2 (pIdentifier .>> spaces .>> pstring "=" .>> spaces) pExpression
                   (fun r e -> AssignStmt { Rname = r; Expression = e }))

let pCommand: Parser<_, unit> = pAssignStmt
                                <|> pListStmt
                                <|> pQuitStmt
                                <|> pPrintStmt
                                <|> pUseStmt
                                <|> (pProjectExpression |>> ProjectExpression)
                                <|> (pDifferenceExpression |>> DifferenceExpression)
