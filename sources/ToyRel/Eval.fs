module Eval

open System.IO
open Common
open Relation
open RelationOp

// Expression evaluator
let rec evalExpression expression =
    match expression with
    | Identifier id -> loadRelation id
    | Expression.ProjectExpression pe -> evalProjectExpression pe
    | Expression.RestrictExpression re -> evalRestrictExpression re
    | Expression.JoinExpression je -> evalJoinExpression je
    | Expression.InfixExpression ie ->
        match ie with
        | DifferenceExpression (expL, expR) -> evalDifferenceExpression expL expR
        | ProductExpression (expL, expR) -> evalProductExpression expL expR

and evalProjectExpression projExp =
    evalExpression projExp.Expression
    |> Result.bind (projectOp projExp.ColumnList)

and evalDifferenceExpression expL expR =
    let diff rel1 rel2 =
        if isUnionCompatible rel1 rel2 then
            differenceOp rel1 rel2
        else
            Result.Error "Relations are not union compatible."

    result {
        let! relL = evalExpression expL
        let! relR = evalExpression expR
        let! rel = diff relL relR
        return rel
    }

and evalRestrictExpression restrictExp =
    evalExpression restrictExp.Expression
    |> Result.bind (restrictOp restrictExp.Condition)

and evalProductExpression expL expR =
    // If there is the duplicate column name between the left and right relation
    // in the product expression, rename the colum name of the right relation.
    // If the right relation is derived from the Identifier expression then
    // prefix the dupulicate column name with "Identifier name + .", otherwise
    // prefix the dupulicate column name with ".".
    let columnPrefix =
        match expR with
        | Identifier (Identifier.Identifier name) -> name + "."
        | _ -> "."

    result {
        let! relL = evalExpression expL
        let! relR = evalExpression expR
        let! rel = productOp relL relR columnPrefix
        return rel
    }

// join (expNameL) (expNameR) (...)
and modifyBinOperand expNameL expNameR (expKeysL: Set<string>) (expKeysR: Set<string>) binOperand =
    match binOperand with
    | Column col ->
        match col with
        | PrefixedColumn ((Identifier.Identifier prefix), (Identifier.Identifier name)) ->
            if (prefix = expNameL && expKeysL.Contains(name)) ||
               (prefix = expNameR && not (expKeysL.Contains(name)) && expKeysR.Contains(name))
            then
                Result.Ok (Column(Column.Identifier(Identifier.Identifier(name))))
            elif (prefix <> expNameL && prefix <> expNameR)
            then
                Result.Error (sprintf "No such prefix '%s'." prefix)
            elif (prefix = expNameL && not (expKeysL.Contains(name))) ||
                 (prefix = expNameR && not (expKeysR.Contains(name)))
            then
                Result.Error (sprintf "No such key '%s' in '%s'." name prefix)
            else
                Result.Ok (binOperand)
        | _ ->
            Result.Ok (binOperand)
    | _ ->
        Result.Ok (binOperand)

// Modify BinOperand in JoinExpression condition as needed.
// For example, the left relation named 'Employee' has the following columns:
//   Name EmpId DeptName
// and the right relation named 'Dept' has the following columns:
//   DeptName Manager
// producting these relations, columns look like this:
//   Name EmpId DeptName Dept.DeptName Manager
// then, BinOperand in condition is modified as follows:
//   DeptName --> DeptName
//   Employee.DeptName --> DeptName
//   Dept.DeptName --> Dept.DeptName
//   Eemployee.Name --> Name
//   Dept.Manager --> Manager
and modifyJoinExpCond joinExp =
    let getExpName exp =
        match exp with
        | Identifier (Identifier.Identifier str) -> str
        | _ -> ""

    let getKeysFromExp exp =
        evalExpression exp
        |> Result.map (fun rel -> columnKeys rel |> set)

    result {
        let nameL = getExpName joinExp.ExpressionL
        let nameR = getExpName joinExp.ExpressionR
        let! keysL = getKeysFromExp joinExp.ExpressionL
        let! keysR = getKeysFromExp joinExp.ExpressionR

        let modifyCondAtom condAtom =
            result {
                let! l = modifyBinOperand nameL nameR keysL keysR condAtom.BinOperandL
                let! r = modifyBinOperand nameL nameR keysL keysR condAtom.BinOperandR
                return { condAtom with BinOperandL = l; BinOperandR = r }
            }

        let rec modifySingleCond singleCond =
            match singleCond with
            | Negation ng ->
                modifySingleCond ng
                |> Result.map Negation
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

and evalJoinExpression joinExp =
    result {
        let! modifiedCond = modifyJoinExpCond joinExp
        let! productedRel = evalProductExpression joinExp.ExpressionL joinExp.ExpressionR
        let! rel = restrictOp modifiedCond productedRel
        return rel
    }


// Statement evaluator
let evalListStmt () =
    try
        Directory.GetFiles(databaseDir, "*.csv")
        |> Array.iter (fun f -> printfn "%s" (Path.GetFileNameWithoutExtension f))
        Result.Ok ()
    with
        | err -> Result.Error err.Message

let evalPrintStmt rname =
    loadRelation rname
    |> Result.map print

let evalUseStmt dbname =
    changeDB dbname

let evalAssignStmt assignStmt =
    evalExpression assignStmt.Expression
    |> Result.bind (save assignStmt.Rname)
