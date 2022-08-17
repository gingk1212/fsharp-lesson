module Eval

open System.IO
open Common
open Relation

let rec evalExpression expression =
    match expression with
    | Identifier id -> loadRelation id
    | Expression.ProjectExpression pe -> evalProjectExpression pe
    | Expression.DifferenceExpression de -> evalDifferenceExpression de

and evalProjectExpression projExp =
    let relation = evalExpression projExp.Expression
    project projExp.ColumnList relation

and evalDifferenceExpression diffExp =
    let rel1 = evalExpression diffExp.Expression1
    let rel2 = evalExpression diffExp.Expression2
    if isUnionComparable rel1 rel2 then
        difference rel1 rel2
    else
        failwith "Relations are not union comparable."

let evalListStmt () =
    Directory.GetFiles(databaseDir, "*.csv")
    |> Array.iter (fun f -> printfn "%s" (Path.GetFileNameWithoutExtension f))

let evalPrintStmt rname =
    let relation = loadRelation rname
    print relation

let evalUseStmt dbname =
    changeDB dbname

let evalAssignStmt assignStmt =
    let relation = evalExpression assignStmt.Expression
    save assignStmt.Rname relation
