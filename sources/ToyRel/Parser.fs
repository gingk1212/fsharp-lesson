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

let pNormalColumn =
    (pIdentifier |>> NormalColumn.Identifier)
    <|> pSBracketColumn

let pColumn =
    attempt((pIdentifier .>> pchar '.') .>>. pNormalColumn) |>> PrefixedColumn
    <|> (pNormalColumn |>> NormalColumn)

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
    <|> (pColumn |>> Column)

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

let pInfixExpression =
    pipe3 (pExprInExpr .>> spaces)
          (pstring "difference" <|> pstring "product" <|> pstring "union" <|> pstring "intersect")
          (spaces >>. pExprInExpr)
          (fun l cmd r ->
               match cmd with
               | "difference" -> DifferenceExpression (l, r)
               | "product" -> ProductExpression (l, r)
               | "union" -> UnionExpression (l, r)
               | "intersect" -> IntersectExpression (l, r)
               | _ -> failwith "not reachable")

let pProjectExpression =
    pipe2 (pstring "project" >>. spaces >>. pExprInExpr .>> spaces) pColumnList
          (fun e c -> { Expression = e; ColumnList = c })

let pRestrictExpression =
    pipe2 (pstring "restrict" >>. spaces >>. pExprInExpr .>> spaces)
          (pchar '(' >>. pCondition .>> pchar ')')
          (fun e c -> { Expression = e; Condition = c })

let pJoinExpression =
    pipe3 (pstring "join" >>. spaces >>. pExprInExpr .>> spaces)
          (pExprInExpr .>> spaces)
          (pchar '(' >>. pCondition .>> pchar ')')
          (fun l r c -> { ExpressionL = l; ExpressionR = r; Condition = c })

let pRenameExpression =
    pipe3 (pstring "rename" >>. spaces >>. pchar '(' >>. pIdentifier .>> pchar '.')
          (pNormalColumn .>> pchar ')' .>> spaces)
          pNormalColumn
          (fun ident oldCol newCol ->
                { Expression = Identifier(ident); OldName = oldCol; NewName = newCol })

// Since it is difficult to distinguish whether the right-hand sides of the
// following statements are Identifier or DifferenceExpression, apply attempt
// to pIdentifier.
// > hoge = (Employee)
// > hoge = (project (Employee) DeptName) difference (project (Dept) DeptName)
pExpressionRef.Value <-
    attempt(pstring "(" >>. pIdentifier .>> pstring ")" |>> Expression.Identifier)
    <|> (pInfixExpression |>> Expression.InfixExpression)
    <|> (pProjectExpression |>> Expression.ProjectExpression)
    <|> (pRestrictExpression |>> Expression.RestrictExpression)
    <|> (pJoinExpression |>> Expression.JoinExpression)
    <|> (pRenameExpression |>> Expression.RenameExpression)


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
let pCommand: Parser<_, unit> = (pInfixExpression |>> InfixExpression)
                                <|> (pProjectExpression |>> ProjectExpression)
                                <|> (pRestrictExpression |>> RestrictExpression)
                                <|> (pJoinExpression |>> JoinExpression)
                                <|> (pRenameExpression |>> RenameExpression)
                                <|> pListStmt
                                <|> pQuitStmt
                                <|> pPrintStmt
                                <|> pUseStmt
                                <|> pAssignStmt
