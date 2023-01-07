open System
open FParsec
open ReCJKLine
open Common
open Relation
open RelationOp
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
        | RestrictExpression restrictExp ->
            evalRestrictExpression restrictExp
            |> Result.bind saveWithRandomName
            |> checkRelationResult
        | JoinExpression joinExp ->
            evalJoinExpression joinExp
            |> Result.bind saveWithRandomName
            |> checkRelationResult
        | RenameExpression renameExp ->
            evalRenameExpression renameExp
            |> Result.bind saveWithRandomName
            |> checkRelationResult
        | InfixExpression infixExp ->
            match infixExp with
            | DifferenceExpression (relL, relR) ->
                evalInfixExpression differenceOp relL relR
                |> Result.bind saveWithRandomName
                |> checkRelationResult
            | ProductExpression (relL, relR) ->
                evalProductExpression relL relR
                |> Result.bind saveWithRandomName
                |> checkRelationResult
            | UnionExpression (relL, relR) ->
                evalInfixExpression unionOp relL relR
                |> Result.bind saveWithRandomName
                |> checkRelationResult
            | IntersectExpression (relL, relR) ->
                evalInfixExpression intersectOp relL relR
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
    let recjk = new ReCJKLine()

    let rec repl () =
        let command = recjk.ReadLine("> ")
        execute command
        repl ()

    repl ()

    0
