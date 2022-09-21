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

changeDB (Identifier.Identifier "wikipedia")

testProductExpression "(Employee) product (Dept)"
|> shouldOk
|> rowCount
|> should 15
