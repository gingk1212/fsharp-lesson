// 課題8: PlayDeedleのprojectとfilterをArguでコマンドライン化

open Argu
open Deedle

type Arguments =
    | Project of string list
    | Filter of string list

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Project _ -> ""
            | Filter _ -> ""

let filter fn rows =
    rows |> Series.filterValues fn |> Frame.ofRows

let project (frame: Frame<int, string>) (names: string list)  =
    frame.Columns.[ names ]

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Arguments>(programName="PlayArgu")
    let res = parser.Parse(inputs=args, ignoreUnrecognized=true)
    let mutable df = Frame.ReadCsv "../../data/シラバス.csv"

    if res.Contains Filter then
        let filters = res.GetResult Filter
        if filters.Length >= 2 then
            df <- df.RowsDense|> filter (fun row ->
                let column = row.TryGetAs<string>(filters[0])
                if column.HasValue then
                    column.Value = filters[1]
                else
                    false
            )

    if res.Contains Project then
        let projects = res.GetResult Project
        if projects.Length > 0 then
            df <- project df projects

    df.Print()

    0
