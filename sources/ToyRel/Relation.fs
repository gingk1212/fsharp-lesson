module Relation

open Deedle
open Common

type T = Relation of Frame<int, string>

let fromFrame (df: Frame<int, string>) =
    df.RowsDense.Values |> Seq.distinct |> Series.ofValues |> Frame.ofRows |> Relation

let loadRelation name =
    let csv = databaseDir + name + ".csv"
    Frame.ReadCsv csv |> fromFrame

let project (columnList: string list) (Relation df) =
    df.Columns.[columnList] |> Relation

let print (Relation df) =
    df.Print()

let save name (Relation df) =
    df.SaveCsv (databaseDir + name + ".csv", includeRowKeys=false)

let columnCount (Relation df) =
    df.ColumnCount
