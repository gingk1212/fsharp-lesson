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


let productOp relL relR prefix =
    let setL = columnKeys relL |> set

    relR
    |> mapColKeys (fun key ->
        if setL.Contains key then
            prefix + key
        else
            key)
    |> product relL


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

        Result.Ok func
    else
        Result.Error "Relations are not theta-comparable."


// This function works recursively because the restrict expression supports the
// nesting of "not".
let rec singleCondToFunc singleCond inv rel =
    match singleCond with
    | Negation negation ->
        singleCondToFunc negation (not inv) rel
    | CondAtom condAtom ->
        condAtomToFunc condAtom rel
        |> Result.map (fun condAtomFunc ->
            if inv then
                fun row -> not (condAtomFunc row)
            else
                condAtomFunc)


// Convert Condition of the restrict expression into a function passed to
// Relation.filter.
// When the condition's type is an infix, it is processed recursively. lastFunc
// and lastLogicalOp is required in the recursive process. lastFunc is a
// function that has already been converted. lastLogicalOp is And/Or operator
// that combines lastFunc and CondAtom which is converted to a function in that
// time.
// For example, the following condition
//   ([hoge]<>3) and ([fuga]>5) or ([bar]<=12)
// is converted into the following function.
//   fun row -> row["hoge"] <> 3 && row["fuga"] > 5 || row["bar"] <= 12
let rec condToFunc cond lastFunc lastLogicalOp rel =
    let joinConds func =
        match lastLogicalOp with
        | And ->
            fun row -> lastFunc row && func row
        | Or ->
            fun row -> lastFunc row || func row

    match cond with
    | InfixCondition infixCond ->
        singleCondToFunc infixCond.SingleCondition false rel
        |> Result.bind (fun func ->
            let condFunc = joinConds func
            condToFunc infixCond.Condition condFunc infixCond.LogicalOp rel)
    | SingleCondition singleCond ->
        singleCondToFunc singleCond false rel
        |> Result.map (fun func ->
            joinConds func)


let restrictOp cond rel =
    condToFunc cond (fun _ -> true) And rel
    |> Result.bind (fun condFunc ->
        filter condFunc rel)
