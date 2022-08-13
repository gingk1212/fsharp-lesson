module Common

open System

type Identifier = string

type Expression =
    | Identifier of Identifier
    | ProjectExpression of ProjectExpression

and ProjectExpression =
    { Expression: Expression 
      ColumnList: string list }

type AssignStmt =
    { Rname: Identifier
      Expression: Expression }

type Command =
    | ProjectExpression of ProjectExpression
    | ListStmt of unit
    | PrintStmt of Identifier
    | AssignStmt of AssignStmt

let databaseDir = "database/master/"

let rand = Random()

let createBaseName () =
    let prefix = "zz"
    let randChar _ = char (rand.Next( (int 'a'), (int 'z')+1 ))
    let randStr = Seq.init 4 randChar |> String.Concat
    prefix + randStr
