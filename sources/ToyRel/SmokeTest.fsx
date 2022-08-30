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
testRestrictExpression "restrict (auction) ([sell_price]>[purchase_price])"
|> shouldOk
|> rowCount
|> should 3

testRestrictExpression "restrict (auction) ([date_bought]=[date_sold])"
|> shouldOk
|> rowCount
|> should 0

testRestrictExpression "restrict (auction) ([reference]=\"R005\")"
|> shouldOk
|> rowCount
|> should 1

testRestrictExpression "restrict (auction) (([reference]<>\"R005\") and ([purchase_price]>5))"
|> shouldOk
|> rowCount
|> should 2

testRestrictExpression "restrict (auction) (([reference]<>\"R005\") and ([purchase_price]>5) or ([sell_price]<=12))"
|> shouldOk
|> rowCount
|> should 4

testRestrictExpression "restrict (auction) (not [sell_price]>[purchase_price])"
|> shouldOk
|> rowCount
|> should 3

testRestrictExpression "restrict (auction) (not ([sell_price]>[purchase_price]))"
|> shouldOk
|> rowCount
|> should 3

testRestrictExpression "restrict (auction) (([reference]<>\"R005\") and not ([purchase_price]>5))"
|> shouldOk
|> rowCount
|> should 3

testRestrictExpression "restrict (auction) (([reference]<>\"R005\") and not ([purchase_price]>5) or ([sell_price]<=12))"
|> shouldOk
|> rowCount
|> should 5

// Error
testRestrictExpression "restrict (auction) ([purchase_price]>\"hoge\")"
|> shouldError

testRestrictExpression "restrict (auction) ([reference]=1)"
|> shouldError

testRestrictExpression "restrict (auction) ([purchase_price]=[reference])"
|> shouldError

testRestrictExpression "restrict (auction) (1>[purchase_price])"
|> shouldError

testRestrictExpression "restrict (auction) (\"R005\"=[reference])"
|> shouldError

testRestrictExpression "restrict (auction) ([NOTHING]=\"R005\")"
|> shouldError

changeDB (Identifier.Identifier "wikipedia")


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
parseAssignStmt "hoge = (Employee)"
|> fun a ->
    evalExpression a.Expression
    |> shouldOk
    |> columnCount
    |> should 3

    let (Identifier.Identifier rname) = a.Rname
    rname |> should "hoge"

parseAssignStmt "fuga = project (Employee) Name, DeptName"
|> fun a ->
    evalExpression a.Expression
    |> shouldOk
    |> columnCount
    |> should 2

    let (Identifier.Identifier rname) = a.Rname
    rname |> should "fuga"

parseAssignStmt "r2 = (project (Employee) DeptName) difference (project (Dept) DeptName)"
|> fun a ->
    evalExpression a.Expression
    |> shouldOk
    |> rowCount
    |> should 1

    let (Identifier.(*  *)Identifier rname) = a.Rname
    rname |> should "r2"

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
