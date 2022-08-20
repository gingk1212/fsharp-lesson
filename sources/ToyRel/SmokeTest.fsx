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

changeDB (Identifier.Identifier "wikipedia")

//
// ProjectExpression test
//
match parseCommand "project (Employee) Name, EmpId, DeptName" with
| ProjectExpression p ->
    evalProjectExpression p
    |> shouldOk
    |> columnCount
    |> should 3
| _ ->
    raiseToyRelException "Parsing result should be 'ProjectExpression'"

match parseCommand "project (project (Employee) Name, EmpId, DeptName) Name, EmpId" with
| ProjectExpression p -> 
    evalProjectExpression p
    |> shouldOk
    |> columnCount
    |> should 2
| _ ->
    raiseToyRelException "Parsing result should be 'ProjectExpression'"

changeDB (Identifier.Identifier "library")
match parseCommand "project (book) author" with
| ProjectExpression p ->
    evalProjectExpression p
    |> shouldOk
    |> rowCount
    |> should 7
| _ ->
    raiseToyRelException "Parsing result should be 'ProjectExpression'"
changeDB (Identifier.Identifier "wikipedia")


//
// DifferenceExpression test
//
match parseCommand "(project (Employee) DeptName) difference (project (Dept) DeptName)" with
| DifferenceExpression d ->
    evalDifferenceExpression d
    |> shouldOk
    |> rowCount
    |> should 1
| _ ->
    raiseToyRelException "Parsing result should be 'DifferenceExpression'"

match parseCommand "(project (Employee) EmpId) difference (project (EmployeeTypeMismatch) EmpId)" with
| DifferenceExpression d ->
    evalDifferenceExpression d
    |> shouldError
| _ ->
    raiseToyRelException "Parsing result should be 'DifferenceExpression'"


//
// PrintStmt test
//
match parseCommand "print Employee" with
| PrintStmt _ ->
    ()
| _ ->
    raiseToyRelException "Parsing result should be 'PrintStmt'"


//
// AssignStmt test
//
match parseCommand "hoge = (Employee)" with
| AssignStmt a ->
    evalExpression a.Expression
    |> shouldOk
    |> columnCount
    |> should 3

    let (Identifier.Identifier rname) = a.Rname
    rname |> should "hoge"
| _ ->
    raiseToyRelException "Parsing result should be 'AssignStmt'"

match parseCommand "fuga = project (Employee) Name, DeptName" with
| AssignStmt a ->
    evalExpression a.Expression
    |> shouldOk
    |> columnCount
    |> should 2

    let (Identifier.Identifier rname) = a.Rname
    rname |> should "fuga"
| _ ->
    raiseToyRelException "Parsing result should be 'AssignStmt'"

match parseCommand "r2 = (project (Employee) DeptName) difference (project (Dept) DeptName)" with
| AssignStmt a ->
    evalExpression a.Expression
    |> shouldOk
    |> rowCount
    |> should 1

    let (Identifier.(*  *)Identifier rname) = a.Rname
    rname |> should "r2"
| _ ->
    raiseToyRelException "Parsing result should be 'AssignStmt'"

parseCommandWithFailure "project = (Employee)"

parseCommandWithFailure "list = (Employee)"

parseCommandWithFailure "foo = Employee"


//
// ListStmt test
//
match parseCommand "list" with
| ListStmt ->
    ()
| _ ->
    raiseToyRelException "Parsing result should be 'ListStmt'"


//
// QuitStmt test
//
match parseCommand "quit" with
| QuitStmt ->
    ()
| _ ->
    raiseToyRelException "Parsing result should be 'QuitStmt'"


//
// UseStmt test
//
match parseCommand "use library" with
| UseStmt u ->
    evalUseStmt u
    |> shouldOk

    let (Identifier.Identifier dbname) = u
    baseDir + dbname + "/" |> should databaseDir
    changeDB (Identifier.Identifier "wikipedia")
| _ ->
    raiseToyRelException "Parsing result should be 'UseStmt'"
