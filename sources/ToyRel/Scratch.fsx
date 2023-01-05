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
// rename (シラバス.専門) 科目
//

// let testCmd = parseCommand "rename (シラバス.専門) 科目"
let testCmd = parseCommand "rename (シラバス.[専門]) 科目"

let renameExp =
    match testCmd with
    | RenameExpression rename -> rename
    | _ -> failwith "error"


// let exp = Identifier(Identifier.Identifier("シラバス"))
// // let oldName = NormalColumn.Identifier(Identifier.Identifier("専門"))
// // let newName = NormalColumn.Identifier(Identifier.Identifier("科目"))
// let oldName = SBracketColumn("専門")
// let newName = SBracketColumn("科目")
// let renameExp = { Expression = exp; OldName = oldName; NewName = newName }


evalRenameExpression renameExp




