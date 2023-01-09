module Common

open System
open System.IO

type Identifier = Identifier of string

type NormalColumn =
    | Identifier of Identifier
    | SBracketColumn of string

type Column =
    | NormalColumn of NormalColumn
    | PrefixedColumn of Identifier * NormalColumn

type ColumnType =
    | Int of int
    | Str of string
    | Invalid

type BinOp = BinOp of string

type BinOperand =
    | Primitive of ColumnType
    | Column of Column

type CondAtom =
    { BinOperandL: BinOperand
      BinOperandR: BinOperand
      BinOp: BinOp }

type SingleCondition =
    | Negation of SingleCondition
    | CondAtom of CondAtom

type LogicalOp =
    | And
    | Or

type Condition =
    | InfixCondition of InfixCondition
    | SingleCondition of SingleCondition

and InfixCondition =
    { SingleCondition: SingleCondition
      LogicalOp: LogicalOp
      Condition: Condition }

type Expression =
    | Identifier of Identifier
    | ProjectExpression of ProjectExpression
    | InfixExpression of InfixExpression
    | RestrictExpression of RestrictExpression
    | JoinExpression of JoinExpression
    | RenameExpression of RenameExpression

and InfixExpression =
    | DifferenceExpression of Expression * Expression
    | ProductExpression of Expression * Expression
    | UnionExpression of Expression * Expression
    | IntersectExpression of Expression * Expression

and ProjectExpression =
    { Expression: Expression
      ColumnList: Column list }

and RestrictExpression =
    { Expression: Expression
      Condition: Condition }

and JoinExpression =
    { ExpressionL: Expression
      ExpressionR: Expression
      Condition: Condition }

and RenameExpression =
    { Expression: Expression
      OldName: NormalColumn
      NewName: NormalColumn }

type AssignStmt =
    { Rname: Identifier
      Expression: Expression }

type Command =
    | InfixExpression of InfixExpression
    | ProjectExpression of ProjectExpression
    | RestrictExpression of RestrictExpression
    | JoinExpression of JoinExpression
    | RenameExpression of RenameExpression
    | ListStmt
    | QuitStmt
    | PrintStmt of Identifier
    | UseStmt of Identifier
    | AssignStmt of AssignStmt

let baseDir = Directory.GetCurrentDirectory () + "/"
let mutable databaseDir = baseDir + "master/"   // default database is "master"

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

let getNameFromNormalColumn col =
    match col with
    | NormalColumn.Identifier (Identifier.Identifier i) -> i
    | NormalColumn.SBracketColumn s -> s

let getNameFromColumn col =
    match col with
    | NormalColumn n ->
        getNameFromNormalColumn n
    | PrefixedColumn (Identifier.Identifier prefix, col) ->
        $"{prefix}.{getNameFromNormalColumn col}"

type ResultBuilder() =
    member this.Bind(m, f) = Result.bind f m
    member this.Return(x) = Result.Ok x

let result = new ResultBuilder()

let mutable lastRname = Identifier.Identifier ""

let lastRnameIsEmpty () =
    let (Identifier.Identifier last) = lastRname
    if last = "" then true else false
