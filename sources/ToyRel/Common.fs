module Common

type Expression =
    | Identifier of string
    | ProjectExpression of ProjectExpression
and ProjectExpression =
    { Expression: Expression 
      ColumnList: string list }

type AssignStmt =
    { Rname: string
      Expression: Expression }

type Command =
    | ProjectExpression of ProjectExpression
    | ListStmt of string
    | PrintStmt of string
    | AssignStmt of AssignStmt

let databaseDir = "database/master/"
