module RelationOp

open Common
open Relation

let isUnionCompatible rel1 rel2 =
    let compKeys () =
        (columnKeys rel1, columnKeys rel2)
        ||> Seq.compareWith (fun col1 col2 -> col1.CompareTo col2)

    let compTypes () =
        (columnTypes rel1, columnTypes rel2)
        ||> Seq.compareWith (fun type1 type2 ->
            if type1.Equals type2 then 0
            else 1)

    if compKeys () <> 0 || compTypes () <> 0 then false
    else true


let getColumnType col rel =
    let colName = getNameFromColumn col
    let colTypes = project [ colName ] rel
                    |> columnTypes
                    |> Seq.toList
    if colTypes.Length = 0 then
        Invalid
    else
        match colTypes.[0] with
        | t when t = typeof<int> -> Int 0
        | t when t = typeof<string> -> Str ""
        | _ -> Invalid


let projectOp columnList rel =
    try
        let colNames = columnList |> List.map (fun column ->
            match column with
            | Column.Identifier (Identifier.Identifier id) -> id
            | SBracketColumn sb -> sb
        )
        Result.Ok (project colNames rel)
    with
        | err -> Result.Error err.Message


let differenceOp rel1 rel2 =
    try
        let rel2Set = rowsHashSet rel2
        filter (fun row -> not (rel2Set.Contains(row))) rel1
    with
        | err -> Result.Error err.Message


let isThetaComparable binOperandL binOperandR rel =
    let check l r =
        match l, r with
        | Int _, Int _
        | Str _, Str _ -> true

        | Int _, Str _
        | Int _, Invalid
        | Str _, Int _
        | Str _, Invalid
        | Invalid, Int _
        | Invalid, Str _
        | Invalid, Invalid -> false

    match binOperandL, binOperandR with
    | Primitive l, Primitive r ->
        check l r
    | Column l, Primitive r ->
        check (getColumnType l rel) r
    | Primitive l, Column r ->
        check l (getColumnType r rel)
    | Column l, Column r ->
        check (getColumnType l rel) (getColumnType r rel)


let condAtomToFunc condAtom rel =
    let compare left right =
        let (BinOp binop) = condAtom.BinOp
        let result =
            match binop with
            | "<=" ->
                left <= right
            | "=" ->
                left = right
            | ">=" ->
                left >= right
            | "<>" ->
                left <> right
            | "<" ->
                left < right
            | ">" ->
                left > right
            | _ ->
                failwithf "\"%sValue\" is not a binary operator." binop

        if condAtom.Not then
            not result
        else
            result

    if isThetaComparable condAtom.BinOperandL condAtom.BinOperandR rel then
        let func row =
            match condAtom.BinOperandL, condAtom.BinOperandR with
            | Primitive primL, Primitive primR ->
                match primL, primR with
                | Int intL, Int intR ->
                    compare intL intR
                | Str strL, Str strR ->
                    compare strL strR
                | _ -> false
            | Primitive prim, Column col | Column col, Primitive prim ->
                match prim with
                | Int i ->
                    let colValue =
                        getNameFromColumn col |> getColumnValue row 
                    match condAtom.BinOperandL with
                    | Primitive _ ->
                        compare i colValue
                    | Column _ ->
                        compare colValue i
                | Str s ->
                    let colValue =
                        getNameFromColumn col |> getColumnValue row 
                    match condAtom.BinOperandL with
                    | Primitive _ ->
                        compare s colValue
                    | Column _ ->
                        compare colValue s
                | _ -> false
            | Column colL, Column colR ->
                let colLValue =
                    getNameFromColumn colL |> getColumnValue row 
                let colRValue =
                    getNameFromColumn colR |> getColumnValue row 
                compare colLValue colRValue

        Result.Ok (func)
    else
        Result.Error "Relations are not theta-comparable."


let rec condToFunc cond lastFunc lastBoolOp rel =
    let joinConds condAtomFunc =
        match lastBoolOp with
        | And ->
            fun row -> lastFunc row && condAtomFunc row
        | Or ->
            fun row -> lastFunc row || condAtomFunc row

    match cond with
    | Conditions conds ->
        condAtomToFunc conds.CondAtom rel
        |> Result.bind (fun condAtomFunc ->
            let condFunc = joinConds condAtomFunc
            condToFunc conds.Condition condFunc conds.BoolOp rel)
    | CondAtom ca ->
        condAtomToFunc ca rel
        |> Result.map (fun condAtomFunc ->
            joinConds condAtomFunc)


let restrictOp cond rel =
    condToFunc cond (fun _ -> true) And rel
    |> Result.bind (fun condFunc ->
        filter condFunc rel)
