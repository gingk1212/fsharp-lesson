#r "nuget: FParsec"
#r "nuget: Deedle"

open FParsec
open Deedle

#load "Deedle.fsx"

// 課題4: module Relationを作りADTしよう
module Relation =
    type T = Relation of Frame<int, string>

    let fromFrame (df: Frame<int, string>) =
        df.RowsDense.Values |> Seq.distinct |> Series.ofValues |> Frame.ofRows |> Relation

    let fromCsv csv =
        Frame.ReadCsv csv |> fromFrame

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
    | Identifier id -> Relation.fromCsv ("database/master/" + id + ".csv")
    | ProjectExpression pe -> evalProjectExpression pe
and evalProjectExpression projExp =
    let (expression, columnList) = projExp
    let (Relation.T.Relation df) = evalExpression expression
    df.Columns.[columnList] |> Relation.fromFrame

let testProjectExpression str =
    match run pProjectExpression str with
        | ParserResult.Success(result, _, _) ->
            let (Relation.T.Relation df) = evalProjectExpression result
            df.Print()
        | Failure(errorMsg, _, _) ->
            printfn "Failure: %s" errorMsg

testProjectExpression "project (Employee) Name, DeptName]"
testProjectExpression "project (project (Employee) Name, EmpId, DeptName) Name, EmpId"
