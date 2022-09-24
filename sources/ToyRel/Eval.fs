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
    | Expression.RestrictExpression re -> evalRestrictExpression re
    | Expression.InfixExpression ie ->
        match ie with
        | DifferenceExpression (expL, expR) -> evalDifferenceExpression expL expR
        | ProductExpression (expL, expR) -> evalProductExpression expL expR

and evalProjectExpression projExp =
    evalExpression projExp.Expression
    |> Result.bind (projectOp projExp.ColumnList)

and evalDifferenceExpression expL expR =
    let diff rel1 rel2 =
        if isUnionCompatible rel1 rel2 then
            differenceOp rel1 rel2
        else
            Result.Error "Relations are not union compatible."

    result {
        let! relL = evalExpression expL
        let! relR = evalExpression expR
        let! rel = diff relL relR
        return rel
    }

and evalRestrictExpression restrictExp =
    evalExpression restrictExp.Expression
    |> Result.bind (restrictOp restrictExp.Condition)

and evalProductExpression expL expR =
    // If there is the duplicate column name between the left and right relation
    // in the product expression, rename the colum name of the right relation.
    // If the right relation is derived from the Identifier expression then
    // prefix the dupulicate column name with "Identifier name + .", otherwise
    // prefix the dupulicate column name with ".".
    let columnPrefix =
        match expR with
        | Identifier (Identifier.Identifier name) -> name + "."
        | _ -> "."

    result {
        let! relL = evalExpression expL
        let! relR = evalExpression expR
        let! rel = productOp relL relR columnPrefix
        return rel
    }


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
