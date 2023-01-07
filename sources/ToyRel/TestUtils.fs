module TestUtils

open System
open FParsec
open Common
open Relation
open RelationOp
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

let shouldContain key rel =
    let keys = rel |> columnKeys |> set
    if keys.Contains(key) then
        ()
    else
        raiseToyRelException (sprintf "The relation should contain \"%s\"" key)


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
    | InfixExpression i ->
        match i with
        | DifferenceExpression (relL, relR) ->
            evalInfixExpression differenceOp relL relR
        | _ ->
            raiseToyRelException "Parsing result should be 'DifferenceExpression'"
    | _ ->
        raiseToyRelException "Parsing result should be 'DifferenceExpression'"

let testRestrictExpression cmd =
    match parseCommand cmd with
    | RestrictExpression r ->
        evalRestrictExpression r
    | _ ->
        raiseToyRelException "Parsing result should be 'RestrictExpression'"

let testProductExpression cmd =
    match parseCommand cmd with
    | InfixExpression i ->
        match i with
        | ProductExpression (relL, relR) ->
            evalProductExpression relL relR
        | _ ->
            raiseToyRelException "Parsing result should be 'ProductExpression'"
    | _ ->
        raiseToyRelException "Parsing result should be 'ProductExpression'"

let testJoinExpression cmd =
    match parseCommand cmd with
    | JoinExpression j ->
        evalJoinExpression j
    | _ ->
        raiseToyRelException "Parsing result should be 'JoinExpression'"

let testRenameExpression cmd =
    match parseCommand cmd with
    | RenameExpression r ->
        evalRenameExpression r
    | _ -> 
        raiseToyRelException "Parsing result should be 'RenameExpression'"

let testUnionExpression cmd =
    match parseCommand cmd with
    | InfixExpression i ->
        match i with
        | UnionExpression (relL, relR) ->
            evalInfixExpression unionOp relL relR
        | _ ->
            raiseToyRelException "Parsing result should be 'UnionExpression'"
    | _ ->
        raiseToyRelException "Parsing result should be 'UnionExpression'"

let testIntersectExpression cmd =
    match parseCommand cmd with
    | InfixExpression i ->
        match i with
        | IntersectExpression (relL, relR) ->
            evalInfixExpression intersectOp relL relR
        | _ ->
            raiseToyRelException "Parsing result should be 'IntersectExpression'"
    | _ ->
        raiseToyRelException "Parsing result should be 'IntersectExpression'"

let testAssignStmt cmd =
    match parseCommand cmd with
    | AssignStmt a ->
        evalAssignStmt a |> shouldOk
        loadRelation a.Rname
    | _ ->
        raiseToyRelException "Parsing result should be 'AssignStmt'"
