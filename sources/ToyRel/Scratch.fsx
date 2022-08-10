#r "nuget: FParsec"
#r "nuget: Deedle"

open FParsec
open Deedle
open System

#load "Deedle.fsx"

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

// parser
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
pExpressionRef.Value <- pstring "("
                        >>. ((pProjectExpression |>> ProjectExpression) <|> (pIdentifier |>> Identifier))
                        .>> pstring ")"

let pPrintStmt = pstring "print" >>. spaces >>. pIdentifier

type Command =
    | ProjectExpression of ProjectExpression
    | PrintStmt of string

let pCommand = (pProjectExpression |>> ProjectExpression)
               <|> (pPrintStmt |>> PrintStmt)

// evaluator
let rec evalExpression expression =
    match expression with
    | Identifier id -> Relation.loadRelation id
    | Expression.ProjectExpression pe -> evalProjectExpression pe
and evalProjectExpression projExp =
    let (expression, columnList) = projExp
    let relation = evalExpression expression
    Relation.project columnList relation

let evalPrintStmt printStmt =
    let relation = Relation.loadRelation printStmt
    Relation.print relation

// execute command
let rand = Random()

let createBaseName () =
    let prefix = "zz"
    let randChar _ = char (rand.Next( (int 'a'), (int 'z')+1 ))
    let randStr = Seq.init 4 randChar |> String.Concat
    prefix + randStr

let saveWithRandomName relation =
    Relation.save (createBaseName()) relation

let execute command =
    match run pCommand command with
    | ParserResult.Success(result, _, _) ->
        match result with
        | ProjectExpression projExp ->
            let relation = evalProjectExpression projExp
            saveWithRandomName relation
        | PrintStmt printStmt ->
            evalPrintStmt printStmt
    | Failure(errorMsg, _, _) ->
        printfn "Failure: %s" errorMsg

execute "project (Employee) Name, DeptName]"
execute "project (project (Employee) Name, EmpId, DeptName) Name, EmpId"
execute "print シラバス"
