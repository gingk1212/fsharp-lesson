module Relation

open Deedle
open Common

type T = Relation of Frame<int, string>

let fromFrame (df: Frame<int, string>) =
    df.RowsDense.Values |> Seq.distinct |> Series.ofValues |> Frame.ofRows |> Relation

let loadRelation (Identifier.Identifier name) =
    let csv = databaseDir + name + ".csv"
    try
        Result.Ok (Frame.ReadCsv csv |> fromFrame)
    with
        | err -> Result.Error err.Message

let print (Relation df) =
    df.Print()

let save (Identifier.Identifier name) (Relation df) =
    try
        df.SaveCsv (databaseDir + name + ".csv", includeRowKeys=false)
        Result.Ok ()
    with
        | err -> Result.Error err.Message

let columnCount (Relation df) =
    df.ColumnCount

let rowCount (Relation df) =
    df.RowCount

let isUnionComparable (Relation df1) (Relation df2) =
    let compKeys () =
        (df1.ColumnKeys, df2.ColumnKeys)
        ||> Seq.compareWith (fun col1 col2 -> col1.CompareTo col2)

    let compTypes () =
        (df1.ColumnTypes, df2.ColumnTypes) 
        ||> Seq.compareWith (fun type1 type2 -> 
            if type1.Equals type2 then 0
            else 1)

    if compKeys () <> 0 || compTypes () <> 0 then false
    else true

let getColumnType col (Relation df) =
    let colName = getNameFromColumn col
    let colType = df.Columns.[[ colName ]].ColumnTypes |> Seq.toList
    colType.[0]

let checkColumnType tp col rel =
    let colType = getColumnType col rel
    if colType = typeof<System.Object> then
        Result.Error (sprintf "No such column: %s" (getNameFromColumn col))
    else
        colType = tp |> Result.Ok

let isThetaComparable cond rel =
    match cond.BinOperandL, cond.BinOperandR with
    | Int _, Int _ ->
        true |> Result.Ok
    | Int _, Str _ ->
        false |> Result.Ok
    | Int _, Column col ->
        checkColumnType typeof<int> col rel
    | Str _, Int _ ->
        false |> Result.Ok
    | Str _, Str _ ->
        true |> Result.Ok
    | Str _, Column col ->
        checkColumnType typeof<string> col rel
    | Column colL, Int _ ->
        checkColumnType typeof<int> colL rel
    | Column colL, Str _ ->
        checkColumnType typeof<string> colL rel
    | Column colL, Column col ->
        let (Relation df) = rel
        let colName = getNameFromColumn col
        let colType = df.Columns.[[ colName ]].ColumnTypes |> Seq.toList
        checkColumnType colType.[0] colL rel

let filter fn row =
    try
        row
        |> Series.filterValues fn
        |> Frame.ofRows
        |> fromFrame
        |> Result.Ok
    with
        | err -> Result.Error err.Message

let compare left right binop =
    match binop with
    | "<=" ->
        left <= right
    | "=" ->
        left = right
    | ">=" ->
        left >= right
    | "<>" ->
        left <> right
    | "<" ->
        left < right
    | ">" ->
        left > right
    | _ ->
        failwithf "\"%s\" is not ac binary operator." binop

let compareColumnAndLiteral<'T when 'T : comparison> colL right condAtom =
    let (BinOp binop) = condAtom.BinOp
    fun (row: ObjectSeries<string>) ->
        let left = row.GetAs<'T>(colL)
        let result = compare left right binop
        if condAtom.Not then not result
        else result

let compareColumns<'T when 'T : comparison> colL colR condAtom =
    let (BinOp binop) = condAtom.BinOp
    fun (row: ObjectSeries<string>) ->
        let left = row.GetAs<'T>(colL)
        let right = row.GetAs<'T>(colR)
        let result = compare left right binop
        if condAtom.Not then not result
        else result

let genCondAtomFunc condAtom rel =
    let genFunc colL =
        let colLName = getNameFromColumn colL
        match condAtom.BinOperandR with
        | Int intR ->
            compareColumnAndLiteral colLName intR condAtom |> Result.Ok
        | Str (Identifier.Identifier strR) ->
            compareColumnAndLiteral colLName strR condAtom |> Result.Ok
        | Column colR ->
            let colRName = getNameFromColumn colR
            compareColumns colLName colRName condAtom |> Result.Ok

    match condAtom.BinOperandL with
    | Int _ | Str _ ->
        Result.Error "The left side of the condition expression must be column name."
    | Column colL ->
        match isThetaComparable condAtom rel with
        | Result.Ok ok when ok = true ->
            genFunc colL
        | Result.Ok _ ->
            Result.Error "Relations are not theta-comparable."
        | Result.Error err ->
            Result.Error err

let genNewCondFunc lastFunc lastAndOr condAtomFunc =
    match lastAndOr with
    | And ->
        fun row -> lastFunc row && condAtomFunc row
    | Or ->
        fun row -> lastFunc row || condAtomFunc row

let rec genCondFunc lastFunc lastAndOr rightCond rel =
    match rightCond with
    | CondAtomCond cac ->
        genCondAtomFunc cac.CondAtom rel
        |> Result.bind (fun condAtomFunc ->
            let newCondFunc =
                genNewCondFunc lastFunc lastAndOr condAtomFunc
            genCondFunc newCondFunc cac.AndOr cac.Condition rel)
    | CondAtom ca ->
        genCondAtomFunc ca rel
        |> Result.map (fun condAtomFunc ->
            genNewCondFunc lastFunc lastAndOr condAtomFunc)
