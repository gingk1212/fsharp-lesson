#r "nuget: FParsec"
#r "nuget: Deedle"

#load "Deedle.fsx"

open Deedle
open FParsec

#load "Common.fs"
open Common

#load "Relation.fs"
open Relation

#load "RelationOp.fs"
open RelationOp

#load "Eval.fs"
open Eval

#load "Parser.fs"
open Parser

#load "TestUtils.fs"
open TestUtils


//
// (project (Employee) Name, DeptName) union (project (rename (Dept.Manager) Name) Name, DeptName)
//

changeDB (Identifier.Identifier "wikipedia")

let rel1 = testProjectExpression "project (Employee) Name, DeptName" |> shouldOk
let rel2 = testProjectExpression "project (rename (Dept.Manager) Name) Name, DeptName" |> shouldOk


let rel2set = rowsHashSet rel2
let newRel1 = rel1 |> filter (fun row -> not (rel2set.Contains(row))) |> shouldOk


let (Relation df1) = newRel1
let (Relation df2) = rel2

(df1.RowsDense.Values |> Seq.toList) @ (df2.RowsDense.Values |> Seq.toList)
|> List.toSeq
|> Series.ofValues
|> Frame.ofRows
|> fromFrame




// implement

let union (Relation df1) (Relation df2) =
    try
        (df1.RowsDense.Values |> Seq.toList) @ (df2.RowsDense.Values |> Seq.toList)
        |> List.toSeq
        |> Series.ofValues
        |> Frame.ofRows
        |> fromFrame
        |> Result.Ok
    with
        | err -> Result.Error err.Message


let unionOp rel1 rel2 =
    let rel1set = rowsHashSet rel1

    rel2
    |> filter (fun row -> not (rel1set.Contains(row)))
    |> Result.bind (union rel1)


let evalUnionExpression expL expR =
    let union rel1 rel2 =
        if isUnionCompatible rel1 rel2 then
            unionOp rel1 rel2
        else
            Result.Error "Relations are not union compatible."

    result {
        let! relL = evalExpression expL
        let! relR = evalExpression expR
        let! rel = union relL relR
        return rel
    }


// let expL =
//     match parseCommand "project (Employee) Name, DeptName" with
//     | ProjectExpression p -> p |> Expression.ProjectExpression
//     | _  -> failwith "error"
// let expR =
//     match parseCommand "project (rename (Dept.Manager) Name) Name, DeptName" with
//     | ProjectExpression p -> p |> Expression.ProjectExpression
//     | _  -> failwith "error"

let (expL, expR) =
    match parseCommand "(project (Employee) Name, DeptName) union (project (rename (Dept.Manager) Name) Name, DeptName)" with
    | InfixExpression i ->
        match i with
        | UnionExpression (e1, e2) -> (e1, e2)
        | _ -> failwith "error"
    | _ -> failwith "error"

evalUnionExpression expL expR



