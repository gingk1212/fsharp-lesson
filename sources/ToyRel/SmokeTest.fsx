#r "nuget: FParsec"
#r "nuget: Deedle"

#load "Common.fs"
open Common

#load "Relation.fs"
open Relation

#load "RelationOp.fs"
open RelationOp

#load "Eval.fs"
open Eval

#load "Parser.fs"

#load "TestUtils.fs"
open TestUtils

changeDB (Identifier.Identifier "wikipedia")

//
// ProjectExpression test
//
testProjectExpression "project (Employee) Name, EmpId, DeptName"
|> shouldOk
|> columnCount
|> should 3

testProjectExpression "project (project (Employee) Name, EmpId, DeptName) Name, EmpId"
|> shouldOk
|> columnCount
|> should 2

changeDB (Identifier.Identifier "library")

testProjectExpression "project (book) author"
|> shouldOk
|> rowCount
|> should 7

changeDB (Identifier.Identifier "wikipedia")


//
// DifferenceExpression test
//
testDifferenceExpression "(project (Employee) DeptName) difference (project (Dept) DeptName)"
|> shouldOk
|> rowCount
|> should 1

testDifferenceExpression "(project (Employee) EmpId) difference (project (EmployeeTypeMismatch) EmpId)"
|> shouldError


//
// RestrictExpression test
//
changeDB (Identifier.Identifier "library")

// Ok
testRestrictExpression "restrict (auction) (sell_price>purchase_price)"
|> shouldOk
|> rowCount
|> should 3

testRestrictExpression "restrict (auction) (date_bought=date_sold)"
|> shouldOk
|> rowCount
|> should 0

testRestrictExpression "restrict (auction) (reference=\"R005\")"
|> shouldOk
|> rowCount
|> should 1

testRestrictExpression "restrict (auction) ((reference<>\"R005\") and (purchase_price>5))"
|> shouldOk
|> rowCount
|> should 2

testRestrictExpression "restrict (auction) ((reference<>\"R005\") and (purchase_price>5) or (sell_price<=12))"
|> shouldOk
|> rowCount
|> should 5

testRestrictExpression "restrict (auction) (not (sell_price=purchase_price))"
|> shouldOk
|> rowCount
|> should 6

testRestrictExpression "restrict (auction) (not (not (sell_price=purchase_price)))"
|> shouldOk
|> rowCount
|> should 0

testRestrictExpression "restrict (auction) ((reference<>\"R005\") and not (purchase_price>5))"
|> shouldOk
|> rowCount
|> should 3

testRestrictExpression "restrict (auction) ((reference<>\"R005\") and not (purchase_price>5) or (sell_price<=12))"
|> shouldOk
|> rowCount
|> should 6

testRestrictExpression "restrict (auction) (5>purchase_price)"
|> shouldOk
|> rowCount
|> should 3

testRestrictExpression "restrict (auction) (\"R005\"=reference)"
|> shouldOk
|> rowCount
|> should 1

// Error
testRestrictExpression "restrict (auction) (purchase_price>\"hoge\")"
|> shouldError

testRestrictExpression "restrict (auction) (reference=1)"
|> shouldError

testRestrictExpression "restrict (auction) (purchase_price=reference)"
|> shouldError

testRestrictExpression "restrict (auction) (NOTHING=\"R005\")"
|> shouldError

parseCommandWithFailure "restrict (auction) (purchase_price=0.5)"

changeDB (Identifier.Identifier "wikipedia")


//
// ProductExpression test
//
testProductExpression "(Employee) product (Dept)"
|> shouldOk
|> rowCount
|> should 15

testProductExpression "(Employee) product (Dept)"
|> shouldOk
|> columnCount
|> should 5

testProductExpression "(Employee) product (project (Dept) DeptName)"
|> shouldOk
|> rowCount
|> should 15

testProductExpression "(project (Employee) Name, EmpId) product (Dept)"
|> shouldOk
|> rowCount
|> should 15


//
// JoinExpression test
//
testJoinExpression "join (Employee) (Dept) (Employee.DeptName=Dept.DeptName)"
|> shouldOk
|> rowCount
|> should 4

testJoinExpression "join (Employee) (Dept) (Employee.DeptName=\"Finance\")"
|> shouldOk
|> rowCount
|> should 6

testJoinExpression "join (Employee) (Dept) (Dept.DeptName=\"Finance\")"
|> shouldOk
|> rowCount
|> should 5

testJoinExpression "join (Employee) (Dept) (DeptName=\"Finance\")"
|> shouldOk
|> rowCount
|> should 6

testJoinExpression "join (Employee) (Dept) (Dept.Manager=\"George\")"
|> shouldOk
|> rowCount
|> should 5

testJoinExpression "join (project (Employee) Name, DeptName) (Dept) (DeptName=\"Finance\")"
|> shouldOk
|> rowCount
|> should 6


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
testAssignStmt "hoge = (Employee)"
|> shouldOk
|> columnCount
|> should 3

testAssignStmt "fuga = project (Employee) Name, DeptName"
|> shouldOk
|> columnCount
|> should 2

testAssignStmt "r2 = (project (Employee) DeptName) difference (project (Dept) DeptName)"
|> shouldOk
|> rowCount
|> should 1

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
