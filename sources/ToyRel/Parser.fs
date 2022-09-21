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
    let pWithoutParen =
        tuple3 pBinOperand pBinOp pBinOperand

    let pWithParen =
        between (pchar '(') (pchar ')') pWithoutParen

    pWithoutParen <|> pWithParen
    |>> (fun (bleft, binop, bright) ->
            CondAtom { BinOperandL = bleft; BinOperandR = bright; BinOp = binop })

let pSingleCondition, pSingleConditionRef = createParserForwardedToRef()

pSingleConditionRef.Value <-
    (pstring "not" >>. spaces >>. (between (pchar '(') (pchar ')') pSingleCondition) |>> Negation)
    <|> pCondAtom

let pCondition, pConditionRef = createParserForwardedToRef()

let pInfixCondition =
    pipe3 (pSingleCondition .>> spaces)
          ((pstring "or" >>% Or) <|> (pstring "and" >>% And))
          (spaces >>. pCondition)
          (fun singleCond logicalOp cond ->
               InfixCondition { SingleCondition = singleCond; LogicalOp = logicalOp; Condition = cond })

pConditionRef.Value <-
    attempt(pInfixCondition)
    <|> (pSingleCondition |>> SingleCondition)


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
    pipe2 (pstring "restrict" >>. spaces >>. pExprInExpr .>> spaces)
          (pchar '(' >>. pCondition .>> pchar ')')
          (fun e c -> { Expression = e; Condition = c })

let pProductExpression =
    pipe2 pExprInExpr (spaces >>. pstring "product" >>. spaces >>. pExprInExpr)
          (fun l r -> { ExpressionL = l; ExpressionR = r })

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
    <|> (pProductExpression |>> Expression.ProductExpression)


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
                                <|> attempt(pDifferenceExpression |>> DifferenceExpression)
                                <|> (pRestrictExpression |>> RestrictExpression)
                                <|> (pProductExpression |>> ProductExpression)
                                <|> pListStmt
                                <|> pQuitStmt
                                <|> pPrintStmt
                                <|> pUseStmt
                                <|> pAssignStmt
