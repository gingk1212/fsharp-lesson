#r "nuget: FParsec"

open System.Text.RegularExpressions
open FParsec

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let firstIdentifier = "([_@a-zA-Z]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})"
let identifier = "([-_@a-zA-Z0-9]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})*"

// 全体マッチする
Regex.Match("abc", firstIdentifier + identifier)
Regex.Match("_abc123", firstIdentifier + identifier)
Regex.Match("abc_123", firstIdentifier + identifier)
Regex.Match("専門", firstIdentifier + identifier)
Regex.Match("フロア", firstIdentifier + identifier)
Regex.Match("@abc", firstIdentifier + identifier)
Regex.Match("a-bc", firstIdentifier + identifier)
Regex.Match("a", firstIdentifier + identifier)

// 全体マッチしない
Regex.Match("123", firstIdentifier + identifier)
Regex.Match("abc.def", firstIdentifier + identifier)
Regex.Match("abc*", firstIdentifier + identifier)
Regex.Match("abc:def", firstIdentifier + identifier)
Regex.Match("abc def", firstIdentifier + identifier)
Regex.Match("(abc)", firstIdentifier + identifier)
Regex.Match("abc+def", firstIdentifier + identifier)
Regex.Match("-abc", firstIdentifier + identifier)

type Identifier = Identifier of string
let pIdentifier = regex (firstIdentifier + identifier) |>> Identifier
test pIdentifier "@aaa"
test pIdentifier "" // error

type SBracketColumn = SBracketColumn of string
let notSBrackets s = s <> '[' && s <> ']'
let pSBracketColumn = pstring "[" >>. many1Satisfy notSBrackets .>> pstring "]" |>> SBracketColumn

type Column =
    | Identifier of Identifier
    | SBracketColumn of SBracketColumn
let pColumn = (pIdentifier |>> Identifier)
              <|> (pSBracketColumn |>> SBracketColumn)
test pColumn "[aaa]"
test pColumn "@a-aa"
test pColumn "123" // error
test pColumn "" // error
test pColumn "[]" // error

type ColumnList = ColumnList of Column list
let pComma = spaces >>. pstring "," .>> spaces
let pColumnList = sepBy1 pColumn pComma |>> ColumnList
test pColumnList "aa, @aa , [a a/], a"
test pColumnList " a, a" // error
test pColumnList "" // error

type Expression =
    | Ident of Identifier
    | ProjExp of ProjectExpression
and ProjectExpression = ProjectExpression of Expression * ColumnList

let pExpression, pExpressionRef = createParserForwardedToRef()
let pProjectExpression = pstring "project" >>. spaces >>. pExpression .>>. (spaces >>. pColumnList) |>> ProjectExpression
// pExpressionRef.Value <- (pstring "(" >>. pProjectExpression .>> pstring ")" |>> ProjExp)
//                         <|> (pstring "(" >>. pIdentifier .>> pstring ")" |>> Ident)
pExpressionRef.Value <- pstring "(" >>. ((pProjectExpression |>> ProjExp) <|> (pIdentifier |>> Ident)) .>> pstring ")"

test pProjectExpression "project (シラバス) 専門, 学年, [あ い]]"
test pProjectExpression "project (project (シラバス) 専門, 学年, 場所) 専門, 学年" // 文字化けでerror?
test pProjectExpression "project (project (aa) bb, cc, dd) bb, cc"
test pProjectExpression "project (aa) [bb]]]]]]" // エラーになってほしい
