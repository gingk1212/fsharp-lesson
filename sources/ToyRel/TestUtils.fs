module TestUtils

open System
open FParsec
open Common
open Relation
open Eval
open Parser

type ToyRelException(message) = inherit Exception(message)

let raiseToyRelException message =
    raise (ToyRelException(message))

let should left right =
    if left <> right then
        raiseToyRelException (sprintf "Not equal, left=(%A), right=(%A)" left right)

let shouldOk result =
    match result with
    | Result.Ok ok ->
        ok
    | Result.Error _ ->
        raiseToyRelException (sprintf "Result should be Ok, but got: %A" result)

let shouldError result =
    match result with
    | Result.Ok _ ->
        raiseToyRelException (sprintf "Result should be Error, but got: %A" result)
    | Result.Error _ ->
        ()

let parseCommand str =
    match run pCommand str with
    | Success(result, _, _) ->
        result
    | Failure(errorMsg, _, _) ->
        raiseToyRelException (sprintf "Parsing failed: %s" errorMsg)

let parseCommandWithFailure str =
    match run pCommand str with
    | Success(result, _, _) ->
        raiseToyRelException (sprintf "Parsing should fail, but succeeded with: \n%A" result)
    | Failure(_, _, _) ->
        ()

let testProjectExpression cmd =
    match parseCommand cmd with
    | ProjectExpression p ->
        evalProjectExpression p
    | _ ->
        raiseToyRelException "Parsing result should be 'ProjectExpression'"

let testDifferenceExpression cmd =
    match parseCommand cmd with
    | DifferenceExpression d ->
        evalDifferenceExpression d
    | _ ->
        raiseToyRelException "Parsing result should be 'DifferenceExpression'"

let testRestrictExpression cmd =
    match parseCommand cmd with
    | RestrictExpression r ->
        evalRestrictExpression r
    | _ ->
        raiseToyRelException "Parsing result should be 'RestrictExpression'"

let parseAssignStmt cmd =
    match parseCommand cmd with
    | AssignStmt a ->
        a
    | _ ->
        raiseToyRelException "Parsing result should be 'AssignStmt'"

