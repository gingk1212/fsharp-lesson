module Relation

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
