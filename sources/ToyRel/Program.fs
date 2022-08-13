open FParsec
open Common
open Relation
open Eval
open Parser

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
        | PrintStmt rname ->
            evalPrintStmt rname
        | AssignStmt assignStmt ->
            evalAssignStmt assignStmt
    | Failure(errorMsg, _, _) ->
        printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main _ =
    execute "project (Employee) Name, DeptName]"
    execute "project (project (Employee) Name, EmpId, DeptName) Name, EmpId"
    execute "print シラバス"
    execute "hoge = (シラバス)"
    execute "fuga = project (Employee) Name, DeptName"
    execute "foo = シラバス" // should be an error
    execute "list"

    0
