#r "nuget: FParsec"
#r "nuget: Deedle"

#load "Deedle.fsx"

open Deedle
open FParsec

#load "Common.fs"
open Common

#load "Relation.fs"
open Relation

#load "Eval.fs"
open Eval

#load "Parser.fs"
open Parser

#load "TestUtils.fs"
open TestUtils

changeDB (Identifier.Identifier "library")

// Ok
match parseCommand "restrict (auction) ([sell_price]>[purchase_price])" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldOk
    |> rowCount
    |> should 3
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) ([date_bought]=[date_sold])" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldOk
    |> rowCount
    |> should 0
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) ([reference]=\"R005\")" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldOk
    |> rowCount
    |> should 1
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) (([reference]<>\"R005\") and ([purchase_price]>5))" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldOk
    |> rowCount
    |> should 2
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) (([reference]<>\"R005\") and ([purchase_price]>5) or ([sell_price]<=12))" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldOk
    |> rowCount
    |> should 4
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) (not [sell_price]>[purchase_price])" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldOk
    |> rowCount
    |> should 3
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) (not ([sell_price]>[purchase_price]))" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldOk
    |> rowCount
    |> should 3
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) (([reference]<>\"R005\") and not ([purchase_price]>5))" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldOk
    |> rowCount
    |> should 3
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) (([reference]<>\"R005\") and not ([purchase_price]>5) or ([sell_price]<=12))" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldOk
    |> rowCount
    |> should 5
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

// Error
match parseCommand "restrict (auction) ([purchase_price]>\"hoge\")" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldError
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) ([reference]=1)" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldError
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) ([purchase_price]=[reference])" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldError
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) (1>[purchase_price])" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldError
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) (\"R005\"=[reference])" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldError
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"

match parseCommand "restrict (auction) ([NOTHING]=\"R005\")" with
| RestrictExpression r ->
    evalRestrictExpression r
    |> shouldError
| _ ->
    raiseToyRelException "Parsing result should be 'RestrictExpression'"
