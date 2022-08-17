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
    let (Identifier.Identifier rname) = identifier
    rname

let execute command =
    match run pCommand command with
    | Success(result, _, _) ->
        match result with
        | ProjectExpression projExp ->
            let relation = evalProjectExpression projExp
            let rname = saveWithRandomName relation
            printfn "Relation %s returned." rname
        | DifferenceExpression diffExp ->
            let relation = evalDifferenceExpression diffExp
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
            let (Identifier.Identifier rname) = assignStmt.Rname
            printfn "Relation %s returned." rname
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
