module Eval

open System.IO
open Common
open Relation

let rec evalExpression expression =
    match expression with
    | Identifier id -> loadRelation id
    | Expression.ProjectExpression pe -> evalProjectExpression pe

and evalProjectExpression projExp =
    let relation = evalExpression projExp.Expression
    project projExp.ColumnList relation

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
