module Relation

open System.Collections.Generic
open Deedle
open Common

type T = Relation of Frame<int, string>

let fromFrame (df: Frame<int, string>) =
    df.RowsDense.Values |> Seq.distinct |> Series.ofValues |> Frame.ofRows |> Relation

let loadRelation (Identifier.Identifier name) =
    let csv = databaseDir + name + ".csv"
    Frame.ReadCsv csv |> fromFrame

let project columnList (Relation df) =
    let strList = columnList |> List.map (fun column ->
        match column with
        | Column.Identifier (Identifier.Identifier id) -> id
        | SBracketColumn sb -> sb
    )
    df.Columns.[strList] |> fromFrame

let print (Relation df) =
    df.Print()

let save (Identifier.Identifier name) (Relation df) =
    df.SaveCsv (databaseDir + name + ".csv", includeRowKeys=false)

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
    let df2Set = df2.RowsDense.Values |> Seq.toList |> HashSet
    df1.RowsDense
    |> Series.filterValues (fun row -> not (df2Set.Contains(row)))
    |> Frame.ofRows
    |> fromFrame
