open System
open FParsec
open RadLine
open Common
open Relation
open Eval
open Parser

let saveWithRandomName relation =
    let name = createBaseName ()
    save name relation
    name

let execute command =
    match run pCommand command with
    | Success(result, _, _) ->
        match result with
        | ProjectExpression projExp ->
            let relation = evalProjectExpression projExp
            let rname = saveWithRandomName relation
            printfn "Relation %s returned." rname
        | ListStmt ->
            evalListStmt ()
        | QuitStmt ->
            Environment.Exit 0
        | PrintStmt rname ->
            evalPrintStmt rname
        | UseStmt dbname ->
            evalUseStmt dbname
        | AssignStmt assignStmt ->
            evalAssignStmt assignStmt
            printfn "Relation %s returned." assignStmt.Rname
    | Failure(errorMsg, _, _) ->
        printfn "Failure: %s" errorMsg

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
