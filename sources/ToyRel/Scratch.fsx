#r "nuget: FParsec"
#r "nuget: Deedle"

#load "Deedle.fsx"

open FParsec

#load "Common.fs"
open Common

#load "Relation.fs"
open Relation

#load "Eval.fs"
open Eval

#load "Parser.fs"
open Parser

#load "TestUtils.fs"
open TestUtils

changeDB (Identifier.Identifier "wikipedia")

match parseCommand "project (Employee) Name, EmpId, DeptName" with
| ProjectExpression p ->
    evalProjectExpression p |> columnCount |> printfn "%d"
| _ ->
    printfn "fail"
