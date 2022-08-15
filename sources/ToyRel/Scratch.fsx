#r "nuget: FParsec"
#r "nuget: Deedle"

#load "Deedle.fsx"

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

match parseCommand "use library" with
| UseStmt u -> printfn "%s" u
| _ -> printfn "failure"
