module Eval

open System.IO
open Common
open Relation
open RelationOp

// Expression evaluator
let rec evalExpression expression =
    match expression with
    | Identifier id -> loadRelation id
    | Expression.ProjectExpression pe -> evalProjectExpression pe
    | Expression.DifferenceExpression de -> evalDifferenceExpression de
    | Expression.RestrictExpression re -> evalRestrictExpression re

and evalProjectExpression projExp =
    evalExpression projExp.Expression
    |> Result.bind (projectOp projExp.ColumnList)

and evalDifferenceExpression diffExp =
    evalExpression diffExp.Expression1
    |> Result.bind (fun rel1 ->
        evalExpression diffExp.Expression2
        |> Result.bind (fun rel2 ->
            if isUnionCompatible rel1 rel2 then
                differenceOp rel1 rel2
            else
                Result.Error "Relations are not union compatible."))

and evalRestrictExpression restrictExp =
    evalExpression restrictExp.Expression
    |> Result.bind (restrictOp restrictExp.Condition)


// Statement evaluator
let evalListStmt () =
    try
        Directory.GetFiles(databaseDir, "*.csv")
        |> Array.iter (fun f -> printfn "%s" (Path.GetFileNameWithoutExtension f))
        Result.Ok ()
    with
        | err -> Result.Error err.Message

let evalPrintStmt rname =
    loadRelation rname
    |> Result.map print

let evalUseStmt dbname =
    changeDB dbname

let evalAssignStmt assignStmt =
    evalExpression assignStmt.Expression
    |> Result.bind (save assignStmt.Rname)
