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
    |> Result.map (fun _ -> identifier)

type ResultType =
    | Rname of Identifier
    | Unit of unit

let execute command =
    match run pCommand command with
    | Success(result, _, _) ->
        match result with
        | ProjectExpression projExp ->
            evalProjectExpression projExp
            |> Result.bind saveWithRandomName
            |> Result.map Rname
        | RestrictExpression restrictExp ->
            evalRestrictExpression restrictExp
            |> Result.bind saveWithRandomName
            |> Result.map Rname
        | JoinExpression joinExp ->
            evalJoinExpression joinExp
            |> Result.bind saveWithRandomName
            |> Result.map Rname
        | RenameExpression renameExp ->
            evalRenameExpression renameExp
            |> Result.bind saveWithRandomName
            |> Result.map Rname
        | InfixExpression infixExp ->
            match infixExp with
            | DifferenceExpression (relL, relR) ->
                evalInfixExpression differenceOp relL relR
                |> Result.bind saveWithRandomName
                |> Result.map Rname
            | ProductExpression (relL, relR) ->
                evalProductExpression relL relR
                |> Result.bind saveWithRandomName
                |> Result.map Rname
            | UnionExpression (relL, relR) ->
                evalInfixExpression unionOp relL relR
                |> Result.bind saveWithRandomName
                |> Result.map Rname
            | IntersectExpression (relL, relR) ->
                evalInfixExpression intersectOp relL relR
                |> Result.bind saveWithRandomName
                |> Result.map Rname
        | ListStmt ->
            evalListStmt ()
            |> Result.map Unit
        | QuitStmt ->
            Environment.Exit 0
            Result.Ok (Unit ())
        | PrintStmt rname ->
            evalPrintStmt rname
            |> Result.map Unit
        | UseStmt dbname ->
            evalUseStmt dbname
            |> Result.map Unit
        | AssignStmt assignStmt ->
            evalAssignStmt assignStmt
            |> Result.map (fun _ -> Rname (assignStmt.Rname))
    | Failure(errorMsg, _, _) ->
        Result.Error errorMsg

[<EntryPoint>]
let main _ =
    let recjk = new ReCJKLine()

    let rec repl () =
        let command = recjk.ReadLine("> ")
        match execute command with
        | Result.Ok ok ->
            match ok with
            | Rname (Identifier.Identifier rname) -> printfn "Relation %s returned." rname
            | Unit _ -> ()
        | Result.Error err ->
            printfn "[Failure] %s" err

        repl ()

    repl ()

    0
