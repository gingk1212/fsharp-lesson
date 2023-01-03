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

changeDB (Identifier.Identifier "wikipedia")


// > (Employee) product (Dept)
// Name EmpId DeptName Dept.DeptName Manager
// ...
//
// in condition, should be
// DeptName --> DeptName
// Employee.DeptName --> DeptName
// Dept.DeptName --> Dept.DeptName
// Eemployee.Name --> Name
// Dept.Manager --> Manager


let testCmd = parseCommand "join (Employee) (Dept) (not (Employee.DeptName=\"Finance\"))"

let joinExp =
    match testCmd with
    | JoinExpression je -> je
    | _ -> failwith "fail"

evalJoinExpression joinExp

modifyJoinExpCond joinExp


let nameL = "Hoge"
let nameR = "Fuga"
let keysL = ["a"; "b"; "c"] |> set
let keysR = ["c"; "d"; "e"] |> set

let prefix = Identifier.Identifier("Hoge")
let name = Identifier.Identifier("c")
let binOperand = Column(PrefixedColumn(prefix, name))

modifyBinOperand nameL nameR keysL keysR binOperand

