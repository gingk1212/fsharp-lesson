module TestUtils

open System
open FParsec
open Parser

type ToyRelException(message) = inherit Exception(message)

let raiseToyRelException message =
    raise (ToyRelException(message))

let should left right =
    if left <> right then
        raiseToyRelException (sprintf "Not equal, left=(%A), right=(%A)" left right)

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
    | Failure(errorMsg, _, _) ->
        ()
