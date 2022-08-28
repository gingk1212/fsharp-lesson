open System
open FParsec
open RadLine
open Common
open Relation
open Eval
open Parser

let saveWithRandomName relation =
    let identifier = createBaseName ()
    save identifier relation
    |> Result.map (fun () ->
        let (Identifier.Identifier rname) = identifier
        rname)

let checkUnitResult result =
    match result with
    | Result.Ok _ ->
        ()
    | Result.Error err ->
        printfn "[Failure] %s" err

let checkRelationResult result =
    match result with
    | Result.Ok rname ->
        printfn "Relation %s returned." rname
    | Result.Error err ->
        printfn "[Failure] %s" err

let execute command =
    match run pCommand command with
    | Success(result, _, _) ->
        match result with
        | ProjectExpression projExp ->
            evalProjectExpression projExp
            |> Result.bind saveWithRandomName
            |> checkRelationResult
        | DifferenceExpression diffExp ->
            evalDifferenceExpression diffExp
            |> Result.bind saveWithRandomName
            |> checkRelationResult
        | RestrictExpression restrictExp ->
            evalRestrictExpression restrictExp
            |> Result.bind saveWithRandomName
            |> checkRelationResult
        | ListStmt ->
            evalListStmt ()
            |> checkUnitResult
        | QuitStmt ->
            Environment.Exit 0
        | PrintStmt rname ->
            evalPrintStmt rname
            |> checkUnitResult
        | UseStmt dbname ->
            evalUseStmt dbname
            |> checkUnitResult
        | AssignStmt assignStmt ->
            match evalAssignStmt assignStmt with
            | Result.Ok _ ->
                let (Identifier.Identifier rname) = assignStmt.Rname
                printfn "Relation %s returned." rname
            | Result.Error err ->
                printfn "[Failure] %s" err
    | Failure(errorMsg, _, _) ->
        printfn "[Failure] %s" errorMsg

[<EntryPoint>]
let main _ =
    let lineEditor = LineEditor()
    lineEditor.Prompt <- LineEditorPrompt(">", ".")
    lineEditor.KeyBindings.Add<PreviousHistoryCommand>(ConsoleKey.P, ConsoleModifiers.Control)
    lineEditor.KeyBindings.Add<NextHistoryCommand>(ConsoleKey.N, ConsoleModifiers.Control)

    let rec repl () =
        let command = lineEditor.ReadLine(Threading.CancellationToken.None).Result
        execute command
        repl ()

    repl ()

    0
