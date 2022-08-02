#r "nuget:Deedle"

open Deedle

#load "Deedle.fsx"
Frame.ReadCsv "../../data/シラバス.csv"

let df = Frame.ReadCsv "../../data/シラバス.csv"

df.RowsDense
|> Series.filterValues(fun row -> row.GetAs<string>("専門") = "数学")
