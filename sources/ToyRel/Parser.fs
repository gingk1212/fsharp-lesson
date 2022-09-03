module Parser

open FParsec
open Common

// Identifier parser
let firstIdentifier = "([_@a-zA-Z]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})"
let identifier = "([-_@a-zA-Z0-9]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})*"

let pIdentifier =
    regex (firstIdentifier + identifier)
    |>> Identifier.Identifier


// Column parser
let notSBrackets s = s <> '[' && s <> ']'
let pSBracketColumn =
    pstring "[" >>. many1Satisfy notSBrackets .>> pstring "]"
    |>> SBracketColumn

let pColumn =
    (pIdentifier |>> Column.Identifier)
    <|> pSBracketColumn

let pComma = spaces >>. pstring "," .>> spaces
let pColumnList = sepBy1 pColumn pComma


// Condition parser
let pBinOp =
    spaces >>. (pstring "<=")
               <|> (pstring "=")
               <|> (pstring ">=")
               <|> (pstring "<>")
               <|> (pstring "<")
               <|> (pstring ">") .>> spaces |>> BinOp

let pBinOperand =
    let normalChar = satisfy (fun c -> c <> '"')

    (regex ("[0-9]+") |>> int |>> Int |>> Primitive)
    <|> ((pchar '"' >>. (manyChars normalChar) .>> pchar '"') |>> Str |>> Primitive)
    <|> ((pchar '[' >>. pColumn .>> pchar ']') |>> Column)

let pCondAtom =
    let pCondAtomWithoutParen =
        tuple3 pBinOperand pBinOp pBinOperand

    let pCondAtomWithParen =
        between (pchar '(') (pchar ')') pCondAtomWithoutParen

    pipe2 (opt (spaces >>. pstring "not" >>. spaces) |>> Option.isSome)
          (pCondAtomWithoutParen <|> pCondAtomWithParen)
          (fun isNot (bleft, binop, bright) ->
              if isNot then
                  { BinOperandL = bleft; BinOperandR = bright; BinOp = binop; Not = true }
              else
                  { BinOperandL = bleft; BinOperandR = bright; BinOp = binop; Not = false })

let pCondition, pConditionRef = createParserForwardedToRef()

let pConditions =
    pipe3 pCondAtom (spaces >>. (pstring "and" <|> pstring "or")) (spaces >>. pCondition)
          (fun condAtom boolop cond ->
               match boolop with
               | "and" -> Conditions { CondAtom = condAtom; BoolOp = And; Condition = cond }
               | "or" | _ -> Conditions { CondAtom = condAtom; BoolOp = Or; Condition = cond })

pConditionRef.Value <-
    attempt(pConditions)
    <|> (pCondAtom |>> CondAtom)


// Expression parser
let pExpression, pExpressionRef = createParserForwardedToRef()

let pExprInExpr =
    pstring "(" >>. (pExpression <|> (pIdentifier |>> Expression.Identifier)) .>> pstring ")"

let pProjectExpression =
    pipe2 (pstring "project" >>. spaces >>. pExprInExpr .>> spaces) pColumnList
          (fun e c -> { Expression = e; ColumnList = c })

let pDifferenceExpression =
    pipe2 pExprInExpr (spaces >>. pstring "difference" >>. spaces >>. pExprInExpr)
          (fun e1 e2 -> { Expression1 = e1; Expression2 = e2 })

let pRestrictExpression =
    pipe2 (pstring "restrict" >>. spaces >>. pExprInExpr .>> spaces) (pchar '(' >>. pCondition .>> pchar ')')
          (fun e c -> { Expression = e; Condition = c })

// Since it is difficult to distinguish whether the right-hand sides of the
// following statements are Identifier or DifferenceExpression, apply attempt
// to pIdentifier.
// > hoge = (Employee)
// > hoge = (project (Employee) DeptName) difference (project (Dept) DeptName)
pExpressionRef.Value <-
    attempt(pstring "(" >>. pIdentifier .>> pstring ")" |>> Expression.Identifier)
    <|> (pProjectExpression |>> Expression.ProjectExpression)
    <|> (pDifferenceExpression |>> Expression.DifferenceExpression)
    <|> (pRestrictExpression |>> Expression.RestrictExpression)


// Statement parser
let pListStmt =
    pstring "list" .>> eof >>% ListStmt

let pQuitStmt =
    pstring "quit" .>> eof >>% QuitStmt

let pPrintStmt =
    pstring "print" >>. spaces >>. pIdentifier .>> eof |>> PrintStmt

let pUseStmt =
    pstring "use" >>. spaces >>. pIdentifier .>> eof |>> UseStmt

let pAssignStmt =
    pipe2 (pIdentifier .>> spaces .>> pstring "=" .>> spaces) pExpression
          (fun r e -> AssignStmt { Rname = r; Expression = e })
    .>> eof


// Command parser
let pCommand: Parser<_, unit> = (pProjectExpression |>> ProjectExpression)
                                <|> (pDifferenceExpression |>> DifferenceExpression)
                                <|> (pRestrictExpression |>> RestrictExpression)
                                <|> pListStmt
                                <|> pQuitStmt
                                <|> pPrintStmt
                                <|> pUseStmt
                                <|> pAssignStmt
