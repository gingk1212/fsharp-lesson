#r "nuget: FParsec"
#r "nuget: Deedle"

open FParsec
open Deedle

#load "Deedle.fsx"

// 課題4: module Relationを作りADTしよう
// 課題5: まずは指定されたファイル名で保存する関数を作る
let databaseDir = "database/master/"

module Relation =
    type T = Relation of Frame<int, string>

    let fromFrame (df: Frame<int, string>) =
        df.RowsDense.Values |> Seq.distinct |> Series.ofValues |> Frame.ofRows |> Relation

    let loadRelation name =
        let csv = databaseDir + name + ".csv"
        Frame.ReadCsv csv |> fromFrame

    let project (columnList: string list) (Relation df) =
        df.Columns.[columnList] |> Relation

    let print (Relation df) =
        df.Print()

    let save name (Relation df) =
        df.SaveCsv (databaseDir + name + ".csv")

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
    | Identifier id -> Relation.loadRelation id
    | ProjectExpression pe -> evalProjectExpression pe
and evalProjectExpression projExp =
    let (expression, columnList) = projExp
    let relation = evalExpression expression
    Relation.project columnList relation

let testProjectExpression str =
    match run pProjectExpression str with
        | ParserResult.Success(result, _, _) ->
            let relation = evalProjectExpression result
            Relation.print relation
            Relation.save "test" relation
        | Failure(errorMsg, _, _) ->
            printfn "Failure: %s" errorMsg

testProjectExpression "project (Employee) Name, DeptName]"
testProjectExpression "project (project (Employee) Name, EmpId, DeptName) Name, EmpId"

// 課題6: ランダムのファイル名を生成しよう
open System

let rand = Random()

let createBaseName () =
    let prefix = "zz"
    let randChars = Array.zeroCreate 4
    for i in 0 .. randChars.Length - 1 do
        let randChar = char (rand.Next( (int 'a'), (int 'z')+1))
        Array.set randChars i randChar
    prefix + System.String randChars

createBaseName()
