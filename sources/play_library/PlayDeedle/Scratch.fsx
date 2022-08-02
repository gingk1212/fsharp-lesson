#r "nuget:Deedle"

open Deedle

#load "Deedle.fsx"
Frame.ReadCsv "../../data/シラバス.csv"

let df = Frame.ReadCsv "../../data/シラバス.csv"

df.RowsDense
|> Series.filterValues(fun row -> row.GetAs<string>("専門") = "数学")
|> Frame.ofRows

df.Columns.[ ["場所"; "学年"] ]

let filter fn rows =
    rows |> Series.filterValues fn |> Frame.ofRows

df.RowsDense |> filter (fun row -> row.GetAs<string>("専門") = "数学")

let project (frame: Frame<int, string>) (names: string list)  =
    frame.Columns.[ names ]

project df ["場所"; "学年"]
