#r "nuget: FParsec"
#r "nuget: Deedle"

#load "Common.fs"
open Common

#load "Relation.fs"
open Relation

#load "Eval.fs"
open Eval

#load "Parser.fs"

#load "TestUtils.fs"
open TestUtils

changeDB "wikipedia"

match parseCommand "project (Employee) Name, EmpId, DeptName" with
| ProjectExpression p ->
    evalProjectExpression p |> columnCount |> should 3
| _ ->
    raiseToyRelException "Parsing result should be 'ProjectExpression'"

match parseCommand "project (project (Employee) Name, EmpId, DeptName) Name, EmpId" with
| ProjectExpression p -> 
    evalProjectExpression p |> columnCount |> should 2
| _ ->
    raiseToyRelException "Parsing result should be 'ProjectExpression'"

changeDB "library"
match parseCommand "project (book) author" with
| ProjectExpression p ->
    evalProjectExpression p |> rowCount |> should 7
| _ ->
    raiseToyRelException "Parsing result should be 'ProjectExpression'"
changeDB "wikipedia"

match parseCommand "print Employee" with
| PrintStmt _ ->
    ()
| _ ->
    raiseToyRelException "Parsing result should be 'PrintStmt'"

match parseCommand "hoge = (Employee)" with
| AssignStmt a ->
    evalExpression a.Expression |> ignore
    a.Rname |> should "hoge"
| _ ->
    raiseToyRelException "Parsing result should be 'AssignStmt'"

match parseCommand "fuga = project (Employee) Name, DeptName" with
| AssignStmt a ->
    evalExpression a.Expression |> ignore
    a.Rname |> should "fuga"
| _ ->
    raiseToyRelException "Parsing result should be 'AssignStmt'"

parseCommandWithFailure "foo = Employee"

match parseCommand "list" with
| ListStmt ->
    ()
| _ ->
    raiseToyRelException "Parsing result should be 'ListStmt'"

match parseCommand "quit" with
| QuitStmt ->
    ()
| _ ->
    raiseToyRelException "Parsing result should be 'QuitStmt'"

match parseCommand "use library" with
| UseStmt u ->
    evalUseStmt u
    baseDir + u + "/" |> should databaseDir
| _ ->
    raiseToyRelException "Parsing result should be 'UseStmt'"
