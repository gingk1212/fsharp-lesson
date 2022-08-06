#r "nuget:Deedle"

open Deedle

#load "Deedle.fsx"
Frame.ReadCsv "../../data/シラバス.csv"

let df = Frame.ReadCsv "../../data/シラバス.csv"

// 課題2: 専門が数学の行だけを残そう
// 課題3: 専門が数学の行だけを持ったFrameを作ろう
df.RowsDense
|> Series.filterValues(fun row -> row.GetAs<string>("専門") = "数学")
|> Frame.ofRows

// 課題4: 場所と学年だけのFrameを作ろう
df.Columns.[ ["場所"; "学年"] ]

// 課題5: フィルタとプロジェクションを関数にしよう
let filter fn rows =
    rows |> Series.filterValues fn |> Frame.ofRows

df.RowsDense |> filter (fun row -> row.GetAs<string>("専門") = "数学")

let project (frame: Frame<int, string>) (names: string list)  =
    frame.Columns.[ names ]

project df ["場所"; "学年"]
