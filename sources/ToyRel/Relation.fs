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

let columnKeys (Relation df) =
    df.ColumnKeys

let columnTypes (Relation df) =
    df.ColumnTypes

let rowsHashSet (Relation df) =
    df.RowsDense.Values |> Seq.toList |> HashSet

let project (colNames: string list) (Relation df) =
    df.Columns.[colNames] |> fromFrame

let filter fn (Relation df) =
    try
        df.RowsDense
        |> Series.filterValues fn
        |> Frame.ofRows
        |> fromFrame
        |> Result.Ok
    with
        | err -> Result.Error err.Message

let getColumnValue (row: ObjectSeries<string>) colName =
    row.GetAs(colName)

let mapColKeys f (Relation df) =
    Frame.mapColKeys f df
    |> fromFrame

let product (Relation df1) (Relation df2) =
    try
        df1.RowsDense.Values
        |> Seq.collect(fun value1 ->
            df2.RowsDense.Values
            |> Seq.map value1.Merge)
        |> Series.ofValues
        |> Frame.ofRows
        |> fromFrame
        |> Result.Ok
    with
        | err -> Result.Error err.Message

let join (Relation df1) (Relation df2) =
    try
        (df1.RowsDense.Values |> Seq.toList) @ (df2.RowsDense.Values |> Seq.toList)
        |> List.toSeq
        |> Series.ofValues
        |> Frame.ofRows
        |> fromFrame
        |> Result.Ok
    with
        | err -> Result.Error err.Message
