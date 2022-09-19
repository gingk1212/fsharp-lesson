#r "nuget: FParsec"
#r "nuget: Deedle"

#load "Deedle.fsx"

open Deedle
open FParsec

#load "Common.fs"
open Common

#load "Relation.fs"
open Relation

#load "RelationOp.fs"
open RelationOp

#load "Eval.fs"
open Eval

#load "Parser.fs"
open Parser

#load "TestUtils.fs"
open TestUtils

//
// RestrictExpression test
//
changeDB (Identifier.Identifier "library")

// Ok
testRestrictExpression "restrict (auction) ([sell_price]>[purchase_price])"
|> shouldOk
|> rowCount
|> should 3

testRestrictExpression "restrict (auction) ([date_bought]=[date_sold])"
|> shouldOk
|> rowCount
|> should 0

testRestrictExpression "restrict (auction) ([reference]=\"R005\")"
|> shouldOk
|> rowCount
|> should 1

testRestrictExpression "restrict (auction) (([reference]<>\"R005\") and ([purchase_price]>5))"
|> shouldOk
|> rowCount
|> should 2

testRestrictExpression "restrict (auction) (([reference]<>\"R005\") and ([purchase_price]>5) or ([sell_price]<=12))"
|> shouldOk
|> rowCount
|> should 5

testRestrictExpression "restrict (auction) (not ([sell_price]=[purchase_price]))"
|> shouldOk
|> rowCount
|> should 6

testRestrictExpression "restrict (auction) (not (not ([sell_price]=[purchase_price])))"
|> shouldOk
|> rowCount
|> should 0

testRestrictExpression "restrict (auction) (([reference]<>\"R005\") and not ([purchase_price]>5))"
|> shouldOk
|> rowCount
|> should 3

testRestrictExpression "restrict (auction) (([reference]<>\"R005\") and not ([purchase_price]>5) or ([sell_price]<=12))"
|> shouldOk
|> rowCount
|> should 6

testRestrictExpression "restrict (auction) (5>[purchase_price])"
|> shouldOk
|> rowCount
|> should 3

testRestrictExpression "restrict (auction) (\"R005\"=[reference])"
|> shouldOk
|> rowCount
|> should 1

// Error
testRestrictExpression "restrict (auction) ([purchase_price]>\"hoge\")"
|> shouldError

testRestrictExpression "restrict (auction) ([reference]=1)"
|> shouldError

testRestrictExpression "restrict (auction) ([purchase_price]=[reference])"
|> shouldError

testRestrictExpression "restrict (auction) ([NOTHING]=\"R005\")"
|> shouldError

parseCommandWithFailure "restrict (auction) ([purchase_price]=0.5)"
