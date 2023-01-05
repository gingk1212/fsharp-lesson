module Eval

open System.IO
open Common
open Relation
open RelationOp

// Expression evaluator
let rec evalExpression expression =
    match expression with
    | Identifier id -> loadRelation id
    | Expression.ProjectExpression project -> evalProjectExpression project
    | Expression.RestrictExpression restrict -> evalRestrictExpression restrict
    | Expression.JoinExpression join -> evalJoinExpression join
    | Expression.RenameExpression rename -> evalRenameExpression rename
    | Expression.InfixExpression infix ->
        match infix with
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
and replaceBinOperand expNameL expNameR (expKeysL: Set<string>) (expKeysR: Set<string>) binOperand =
    match binOperand with
    | Column col ->
        match col with
        | PrefixedColumn ((Identifier.Identifier prefix), normalCol) ->
            let colName =
                match normalCol with
                | NormalColumn.Identifier (Identifier.Identifier i) -> i
                | NormalColumn.SBracketColumn s -> s

            if (prefix = expNameL && expKeysL.Contains(colName)) ||
               (prefix = expNameR && not (expKeysL.Contains(colName)) && expKeysR.Contains(colName))
            then
                Result.Ok (Column(NormalColumn(normalCol)))
            elif (prefix <> expNameL && prefix <> expNameR)
            then
                Result.Error (sprintf "No such prefix '%s'." prefix)
            elif (prefix = expNameL && not (expKeysL.Contains(colName))) ||
                 (prefix = expNameR && not (expKeysR.Contains(colName)))
            then
                Result.Error (sprintf "No such key '%s' in '%s'." colName prefix)
            else
                Result.Ok (binOperand)
        | _ ->
            Result.Ok (binOperand)
    | _ ->
        Result.Ok (binOperand)

// Replace BinOperand in JoinExpression condition as needed.
// For example, the left relation named 'Employee' has the following columns:
//   Name EmpId DeptName
// and the right relation named 'Dept' has the following columns:
//   DeptName Manager
// producting these relations, columns look like this:
//   Name EmpId DeptName Dept.DeptName Manager
// then, BinOperand in condition is replaced as follows:
//   DeptName --> DeptName
//   Employee.DeptName --> DeptName
//   Dept.DeptName --> Dept.DeptName
//   Eemployee.Name --> Name
//   Dept.Manager --> Manager
and replaceJoinExpCond joinExp =
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

        let replaceCondAtom condAtom =
            result {
                let! l = replaceBinOperand nameL nameR keysL keysR condAtom.BinOperandL
                let! r = replaceBinOperand nameL nameR keysL keysR condAtom.BinOperandR
                return { condAtom with BinOperandL = l; BinOperandR = r }
            }

        let rec replaceSingleCond singleCond =
            match singleCond with
            | Negation ng ->
                replaceSingleCond ng
                |> Result.map Negation
            | CondAtom ca ->
                replaceCondAtom ca
                |> Result.map CondAtom

        let rec replaceCond cond =
            match cond with
            | InfixCondition ic ->
                result {
                    let! newSc = replaceSingleCond ic.SingleCondition
                    let! newIc = replaceCond ic.Condition
                    return InfixCondition({ ic with SingleCondition = newSc; Condition = newIc })
                }
            | SingleCondition sc ->
                replaceSingleCond sc
                |> Result.map SingleCondition

        let! newCond = replaceCond joinExp.Condition
        return newCond
    }

and evalJoinExpression joinExp =
    result {
        let! newCond = replaceJoinExpCond joinExp
        let! productedRel = evalProductExpression joinExp.ExpressionL joinExp.ExpressionR
        let! rel = restrictOp newCond productedRel
        return rel
    }

and evalRenameExpression renameExp =
    let oldName = getNameFromNormalColumn renameExp.OldName
    let newName = getNameFromNormalColumn renameExp.NewName
    evalExpression renameExp.Expression
    |> Result.map (mapColKeys (fun key -> if key = oldName then newName else key))


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
