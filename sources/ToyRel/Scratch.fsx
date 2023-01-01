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


// > (Employee) product (Dept)
// Name EmpId DeptName Dept.DeptName Manager
// ...
//
// in condition, should be
// DeptName --> DeptName
// Employee.DeptName --> DeptName
// Dept.DeptName --> Dept.DeptName
// Eemployee.Name --> Name
// Dept.Manager --> Manager


//
// getColumnType, getColumnValue にひと手間加える必要がある
// 専用のカラムチェック用ヘルパー関数を用意するのがよさそう
// チェックというより変換か。両関数ともにカラム名を使って目的の値を取得しているからカラム名を適切なものに変換する
//

// let test cmd colName =
//     let (Relation df) =
//         match testProductExpression cmd with
//         | Result.Ok ok -> ok
//         | Result.Error err -> failwithf "err: %s" err

//     df.RowsDense.Values
//     |> Seq.map (fun row ->
//         let colVal = getColumnValue row colName
//         colVal = "Harry")

// --> 左右のrelation名が必要になるからやめる。getColumnTypeはrestrict (relation一つ) でも使うものだから。
// --> 各カラムがどっちのrelに属していたかもわからないし。ここでは。


//
// 事前にConditionの中身を書き換えたものを作成する方針。Eval.fsでやるのがいいか
//

let cmd = parseCommand "restrict (Employee) (DeptName=\"Finance\")"
// let cmd = parseCommand "restrict (Employee) (DeptName=\"Finance\" and DeptName=\"Sales\" and DeptName=\"Human Resources\")"

let origCond =
    match cmd with
    | RestrictExpression re -> re.Condition
    | _ -> failwith "fail"

let origCondAtom =
    match origCond with
    | SingleCondition sc ->
        match sc with
        | CondAtom ca -> ca
        | _ -> failwith "fail"
    | _ -> failwith "fail"

let condAtom =
    { origCondAtom with
        BinOperandL = Column(Column.Identifier(Identifier.Identifier "Employee.DepthName")) }

let cond = SingleCondition(CondAtom(condAtom))


//
// Conditionを適切に書き換える関数を作ってみる。evalJoinExpressionの中で呼ばれる想定
//

let getExpName exp =
    match exp with
    | Identifier (Identifier.Identifier str) -> str
    | _ -> ""

let getKeysFromExp exp =
    evalExpression exp
    |> Result.map (fun rel -> columnKeys rel |> set)

let modifyJoinExpCond joinExp =
    result {
        let nameL = getExpName joinExp.ExpressionL
        let nameR = getExpName joinExp.ExpressionR
        let! keysL = getKeysFromExp joinExp.ExpressionL
        let! keysR = getKeysFromExp joinExp.ExpressionR

        let modifyBinOperand binOperand =
            match binOperand with
            | Column col ->
                match col with
                | PrefixedColumn ((Identifier.Identifier prefix), (Identifier.Identifier name)) ->
                    if (prefix = nameL && keysL.Contains(name)) ||
                       (prefix = nameR && not (keysL.Contains(name)) && keysR.Contains(name))
                    then
                        Result.Ok (Column(Column.Identifier(Identifier.Identifier(name))))
                    elif (prefix <> nameL && prefix <> nameR)
                    then
                        Result.Error (sprintf "No such prefix '%s'." prefix)
                    elif (prefix = nameL && not (keysL.Contains(name))) ||
                         (prefix = nameR && not (keysR.Contains(name)))
                    then
                        Result.Error (sprintf "No such key in '%s'." prefix)
                    else
                        Result.Ok (Column(Column.Identifier(Identifier.Identifier($"{prefix}.{name}"))))
                | _ ->
                    Result.Ok (binOperand)
            | _ ->
                Result.Ok (binOperand)

        let modifyCondAtom condAtom =
            result {
                let! l = modifyBinOperand condAtom.BinOperandL
                let! r = modifyBinOperand condAtom.BinOperandR
                return { condAtom with BinOperandL = l; BinOperandR = r }
            }

        let rec modifySingleCond singleCond =
            match singleCond with
            | Negation ng ->
                modifySingleCond ng
            | CondAtom ca ->
                modifyCondAtom ca
                |> Result.map CondAtom

        let rec modifyCond cond =
            match cond with
            | InfixCondition ic ->
                result {
                    let! newSc = modifySingleCond ic.SingleCondition
                    let! newIc = modifyCond ic.Condition
                    return InfixCondition({ ic with SingleCondition = newSc; Condition = newIc })
                }
            | SingleCondition sc ->
                modifySingleCond sc
                |> Result.map SingleCondition

        let! newCond = modifyCond joinExp.Condition
        return newCond
    }


// let testCmd = parseCommand "restrict (Employee) (Employee.DeptName=\"Finance\")"

let testCmd =
    parseCommand "restrict (Employee)
        (Employee.DeptName=\"Finance\"
         and Dept.DeptName=\"Sales\"
         and DeptName=\"Human Resources\"
         and Dept.Manager=\"George\")"
        //  and Hoge.DeptName= \"Sales\")"

// let testCmd =
//     parseCommand "restrict (project (Employee) Name, EmpId, DeptName)
//         (Dept.DeptName=\"Sales\"
//          and DeptName=\"Human Resources\"
//          and Dept.Manager=\"George\")"

let testCond =
    match testCmd with
    | RestrictExpression re -> re.Condition
    | _ -> failwith "fail"

let joinExp = {
    ExpressionL = Identifier(Identifier.Identifier("Employee"))
    // ExpressionL =
    //     match testCmd with
    //     | RestrictExpression re -> re.Expression
    //     | _ -> failwith "fail"
    ExpressionR = Identifier(Identifier.Identifier("Dept"))
    Condition = testCond
}

modifyJoinExpCond joinExp


let evalJoinExpression joinExp =
    result {
        let! modifiedCond = modifyJoinExpCond joinExp
        let! productedRel = evalProductExpression joinExp.ExpressionL joinExp.ExpressionR
        let! rel = restrictOp modifiedCond productedRel
        return rel
    }

evalJoinExpression joinExp

