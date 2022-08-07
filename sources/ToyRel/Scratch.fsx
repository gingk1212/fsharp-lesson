#r "nuget: FParsec"
#r "nuget: Deedle"

open FParsec
open Deedle

#load "Deedle.fsx"

// 課題3: Relationの型をつくれ
type Relation = Relation of Frame<int, string>

let distinct (df: Frame<int, string>) =
    df.RowsDense.Values |> Seq.distinct |> Series.ofValues |> Frame.ofRows |> Relation

let roadCsvWithDistinct csv =
    Frame.ReadCsv csv |> distinct

let firstIdentifier = "([_@a-zA-Z]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})"
let identifier = "([-_@a-zA-Z0-9]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})*"

let pIdentifier = regex (firstIdentifier + identifier)
let notSBrackets s = s <> '[' && s <> ']'
let pSBracketColumn = pstring "[" >>. many1Satisfy notSBrackets .>> pstring "]"
let pColumn = pIdentifier <|> pSBracketColumn
let pComma = spaces >>. pstring "," .>> spaces
let pColumnList = sepBy1 pColumn pComma

type Expression =
    | Identifier of string
    | ProjectExpression of ProjectExpression
and ProjectExpression = Expression * string list

let pExpression, pExpressionRef = createParserForwardedToRef()
let pProjectExpression = pstring "project" >>. spaces >>. pExpression .>>. (spaces >>. pColumnList)
pExpressionRef.Value <- pstring "(" >>. ((pProjectExpression |>> ProjectExpression) <|> (pIdentifier |>> Identifier)) .>> pstring ")"

let rec evalExpression expression =
    match expression with
    | Identifier id -> roadCsvWithDistinct ("database/master/" + id + ".csv")
    | ProjectExpression pe -> evalProjectExpression pe
and evalProjectExpression projExp =
    let (expression, columnList) = projExp
    let (Relation df) = evalExpression expression
    df.Columns.[columnList] |> distinct

let testProjectExpression str =
    match run pProjectExpression str with
        | ParserResult.Success(result, _, _) ->
            let (Relation df) = evalProjectExpression result
            df.Print()
        | Failure(errorMsg, _, _) ->
            printfn "Failure: %s" errorMsg

testProjectExpression "project (Employee) Name, DeptName]"
testProjectExpression "project (project (Employee) Name, EmpId, DeptName) Name, EmpId"
