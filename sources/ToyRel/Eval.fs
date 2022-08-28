module Eval

open System.IO
open Common
open Relation

// Condition evaluator
let evalCondAtom cond rel =
    restrict cond rel

let rec evalCondition cond rel =
    match cond with
    | AndCond ac -> evalAndCond ac rel
    | OrCond oc -> evalOrCond oc rel
    | CondAtom ca -> evalCondAtom ca rel

and evalAndCond cond rel =
    evalCondAtom cond.CondAtom rel
    |> Result.bind (fun relL ->
        evalCondition cond.Condition rel
        |> Result.bind (fun relR ->
            binOpAnd relL relR))

and evalOrCond cond rel =
    evalCondAtom cond.CondAtom rel
    |> Result.bind (fun relL ->
        evalCondition cond.Condition rel
        |> Result.bind (fun relR ->
            binOpOr relL relR))


// Expression evaluator
let rec evalExpression expression =
    match expression with
    | Identifier id -> loadRelation id
    | Expression.ProjectExpression pe -> evalProjectExpression pe
    | Expression.DifferenceExpression de -> evalDifferenceExpression de
    | Expression.RestrictExpression re -> evalRestrictExpression re

and evalProjectExpression projExp =
    evalExpression projExp.Expression
    |> Result.bind (project projExp.ColumnList)

and evalDifferenceExpression diffExp =
    evalExpression diffExp.Expression1
    |> Result.bind (fun rel1 ->
        evalExpression diffExp.Expression2
        |> Result.bind (fun rel2 ->
            if isUnionComparable rel1 rel2 then
                difference rel1 rel2
            else
                Result.Error "Relations are not union comparable."))

and evalRestrictExpression restrictExp =
    evalExpression restrictExp.Expression
    |> Result.bind (evalCondition restrictExp.Condition)


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
