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

changeDB (Identifier.Identifier "library")

testRestrictExpression "restrict (auction) ([sell_price]>[purchase_price])"
|> shouldOk
|> rowCount
|> should 3

testRestrictExpression "restrict (auction) (([reference]<>\"R005\") and ([purchase_price]>5))"
|> shouldOk
|> rowCount
|> should 2
