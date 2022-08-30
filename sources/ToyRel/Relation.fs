module Relation

open System.Collections.Generic
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

let project columnList (Relation df) =
    try
        let strList = columnList |> List.map (fun column ->
            match column with
            | Column.Identifier (Identifier.Identifier id) -> id
            | SBracketColumn sb -> sb
        )
        Result.Ok (df.Columns.[strList] |> fromFrame)
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

let difference (Relation df1) (Relation df2) =
    try
        let df2Set = df2.RowsDense.Values |> Seq.toList |> HashSet
        df1.RowsDense
        |> Series.filterValues (fun row -> not (df2Set.Contains(row)))
        |> Frame.ofRows
        |> fromFrame
        |> Result.Ok
    with
        | err -> Result.Error err.Message

let private getColumnType col (Relation df) =
    let colName = getNameFromColumn col
    let colType = df.Columns.[[ colName ]].ColumnTypes |> Seq.toList
    colType.[0]

let private checkColumnType tp col rel =
    let colType = getColumnType col rel
    if colType = typeof<System.Object> then
        Result.Error (sprintf "No such column: %s" (getNameFromColumn col))
    else
        colType = tp |> Result.Ok

let private isThetaComparable cond rel =
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

let private filter fn row =
    try
        row
        |> Series.filterValues fn
        |> Frame.ofRows
        |> fromFrame
        |> Result.Ok
    with
        | err -> Result.Error err.Message

let private compareColumnAndLiteral<'T when 'T : comparison> binop colL right isNot (row: ObjectSeries<string>) =
    let colLName = getNameFromColumn colL
    let result = 
        match binop with
        | "<=" ->
            row.GetAs<'T>(colLName) <= right
        | "=" ->
            row.GetAs<'T>(colLName) = right
        | ">=" ->
            row.GetAs<'T>(colLName) >= right
        | "<>" ->
            row.GetAs<'T>(colLName) <> right
        | "<" ->
            row.GetAs<'T>(colLName) < right
        | ">" ->
            row.GetAs<'T>(colLName) > right
        | _ ->
            failwithf "\"%s\" is not a binary operator." binop
    if isNot then not result
    else result

let private compareColumns<'T when 'T : comparison> binop colL colR isNot (row: ObjectSeries<string>) =
    let colRName = getNameFromColumn colR
    let right = row.GetAs<'T>(colRName)
    compareColumnAndLiteral binop colL right isNot row

let restrict condAtom rel =
    let (cond, isNot) =
        match condAtom with
        | CondAtom.CondAtom c ->
            (c, false)
        | CondAtom.CondAtomWithNot c -> 
            (c, true)

    let doRestrict colL =
        let (Relation df) = rel
        let (BinOp binop) = cond.BinOp
        match cond.BinOperandR with
        | Int intR ->
            df.RowsDense
            |> filter (compareColumnAndLiteral binop colL intR isNot)
        | Str (Identifier.Identifier strR) ->
            df.RowsDense
            |> filter (compareColumnAndLiteral binop colL strR isNot)
        | Column colR ->
            match getColumnType colL rel with
            | t when t = typeof<int> ->
                df.RowsDense
                |> filter (compareColumns<int> binop colL colR isNot)
            | t when t = typeof<string> ->
                df.RowsDense
                |> filter (compareColumns<string> binop colL colR isNot)
            | t ->
                Result.Error (sprintf "Invalid column type: %A" t)

    match cond.BinOperandL with
    | Int _ | Str _ ->
        Result.Error "The left side of the condition expression must be column name."
    | Column colL ->
        match isThetaComparable cond rel with
        | Result.Ok ok when ok = true ->
            doRestrict colL
        | Result.Ok _ ->
            Result.Error "Relations are not theta-comparable."
        | Result.Error err ->
            Result.Error err


let binOpAnd relL relR =
    if isUnionComparable relL relR then
        try
            let (Relation dfL) = relL
            let (Relation dfR) = relR
            let dfRSet = dfR.RowsDense.Values |> Seq.toList |> HashSet

            dfL.RowsDense
            |> filter (fun row -> dfRSet.Contains(row))
        with
            | err -> Result.Error err.Message
    else
        Result.Error "Relations are not union comparable."

let binOpOr relL relR =
    if isUnionComparable relL relR then
        try
            let (Relation dfL) = relL
            let (Relation dfR) = relR

            Seq.append dfL.RowsDense.Values dfR.RowsDense.Values
            |> Series.ofValues
            |> Frame.ofRows
            |> fromFrame
            |> Result.Ok
        with
            | err -> Result.Error err.Message
    else
        Result.Error "Relations are not union comparable."
