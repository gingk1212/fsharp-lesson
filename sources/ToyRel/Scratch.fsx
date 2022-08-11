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
and ProjectExpression =
    { Expression: Expression 
      ColumnList: string list }

let pExpression, pExpressionRef = createParserForwardedToRef()

let pBracketIdentifier =
    pstring "(" >>. pIdentifier .>> pstring ")"

let pExprInExpr =
    attempt(pstring "(" >>. pExpression .>> pstring ")")
    <|> (pBracketIdentifier |>> Identifier)

let pProjectExpression =
    pipe2 (pstring "project" >>. spaces >>. pExprInExpr .>> spaces) pColumnList
          (fun e c -> { Expression = e; ColumnList = c })

pExpressionRef.Value <-
    (pProjectExpression |>> ProjectExpression)
    <|> (pBracketIdentifier |>> Identifier)

let pPrintStmt = pstring "print" >>. spaces >>. pIdentifier

type AssignStmt =
    { Rname: string
      Expression: Expression }

let pAssignStmt = 
    pipe2 (pIdentifier .>> spaces .>> pstring "=" .>> spaces) pExpression
          (fun r e -> { Rname = r; Expression = e })

type Command =
    | ProjectExpression of ProjectExpression
    | PrintStmt of string
    | AssignStmt of AssignStmt

let pCommand = (pProjectExpression |>> ProjectExpression)
               <|> (pPrintStmt |>> PrintStmt)
               <|> (pAssignStmt |>> AssignStmt)

// evaluator
let rec evalExpression expression =
    match expression with
    | Identifier id -> Relation.loadRelation id
    | Expression.ProjectExpression pe -> evalProjectExpression pe

and evalProjectExpression projExp =
    let relation = evalExpression projExp.Expression
    Relation.project projExp.ColumnList relation

let evalPrintStmt printStmt =
    let relation = Relation.loadRelation printStmt
    Relation.print relation

let evalAssignStmt assignStmt =
    let relation = evalExpression assignStmt.Expression
    Relation.save assignStmt.Rname relation

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
        | AssignStmt assignStmt ->
            evalAssignStmt assignStmt
    | Failure(errorMsg, _, _) ->
        printfn "Failure: %s" errorMsg

execute "project (Employee) Name, DeptName]"
execute "project (project (Employee) Name, EmpId, DeptName) Name, EmpId"
execute "print シラバス"
execute "hoge = (シラバス)"
execute "fuga = project (Employee) Name, DeptName"
execute "foo = シラバス" // should be an error
