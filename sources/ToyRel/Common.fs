module Common

open System
open System.IO

type Identifier = Identifier of string

type Column =
    | Identifier of Identifier
    | SBracketColumn of string

type BinOp = BinOp of string

type BinOperand =
    | Int of int
    | Str of Identifier
    | Column of Column

type CondAtom =
    { BinOperandL: BinOperand
      BinOperandR: BinOperand
      BinOp: BinOp
      Not: bool }

type Condition =
    | AndCond of CondAtomCond
    | OrCond of CondAtomCond
    | CondAtom of CondAtom

and CondAtomCond =
    { CondAtom: CondAtom
      Condition: Condition }

type Expression =
    | Identifier of Identifier
    | ProjectExpression of ProjectExpression
    | DifferenceExpression of DifferenceExpression
    | RestrictExpression of RestrictExpression

and ProjectExpression =
    { Expression: Expression
      ColumnList: Column list }

and DifferenceExpression =
    { Expression1: Expression
      Expression2: Expression }

and RestrictExpression =
    { Expression: Expression
      Condition: Condition }

type AssignStmt =
    { Rname: Identifier
      Expression: Expression }

type Command =
    | ProjectExpression of ProjectExpression
    | DifferenceExpression of DifferenceExpression
    | RestrictExpression of RestrictExpression
    | ListStmt
    | QuitStmt
    | PrintStmt of Identifier
    | UseStmt of Identifier
    | AssignStmt of AssignStmt

let baseDir = "database/"
let mutable databaseDir = baseDir + "master/"

let changeDB (Identifier.Identifier dbname) =
    let newdbDir = baseDir + dbname + "/"
    if Directory.Exists newdbDir then
        databaseDir <- newdbDir
        Result.Ok ()
    else
        Result.Error (sprintf "No such database: %s" dbname)

let rand = Random()

let createBaseName () =
    let prefix = "zz"
    let randChar _ = char (rand.Next( (int 'a'), (int 'z')+1 ))
    let randStr = Seq.init 4 randChar |> String.Concat
    Identifier.Identifier (prefix + randStr)

let getNameFromColumn col =
    match col with
    | Column.Identifier (Identifier.Identifier i) -> i
    | Column.SBracketColumn s -> s
