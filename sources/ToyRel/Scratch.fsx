#r "nuget: FParsec"
#r "nuget: Deedle"

#load "Deedle.fsx"

open FParsec
open System

#load "Common.fs"
open Common

#load "Relation.fs"
open Relation

#load "Eval.fs"
open Eval

#load "Parser.fs"
open Parser

let rand = Random()

let createBaseName () =
    let prefix = "zz"
    let randChar _ = char (rand.Next( (int 'a'), (int 'z')+1 ))
    let randStr = Seq.init 4 randChar |> String.Concat
    prefix + randStr

let saveWithRandomName relation =
    save (createBaseName()) relation

let execute command =
    match run pCommand command with
    | Success(result, _, _) ->
        match result with
        | ProjectExpression projExp ->
            let relation = evalProjectExpression projExp
            saveWithRandomName relation
        | ListStmt _ ->
            evalListStmt()
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
execute "list"
