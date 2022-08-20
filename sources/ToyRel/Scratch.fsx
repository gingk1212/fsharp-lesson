#r "nuget: FParsec"
#r "nuget: Deedle"

#load "Deedle.fsx"

open Deedle
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

match parseCommand "(project (Employee) DeptName) difference (project (Dept) DeptName)" with
| DifferenceExpression d ->
    evalDifferenceExpression d
    |> Result.map print
    |> printfn "%A"
| _ ->
    printfn "fail"

match parseCommand "(project (Employee) EmpId) difference (project (EmployeeTypeMismatch) EmpId)" with
| DifferenceExpression d ->
    evalDifferenceExpression d
    |> Result.map print
    |> printfn "%A"
| _ ->
    printfn "fail"
