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
