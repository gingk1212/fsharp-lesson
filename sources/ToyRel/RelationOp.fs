module RelationOp

open System.Collections.Generic
open Deedle
open Common
open Relation

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

let restrict cond rel =
    let (Relation df) = rel
    genCondFunc (fun _ -> true) And cond rel
    |> Result.bind (fun condFunc ->
        df.RowsDense
        |> filter condFunc
    )
