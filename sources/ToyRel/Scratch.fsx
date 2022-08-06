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

let pIdentifier = regex (firstIdentifier + identifier)
test pIdentifier "@aaa"
test pIdentifier "" // error

let notSBrackets s = s <> '[' && s <> ']'
let pSBracketColumn = pstring "[" >>. many1Satisfy notSBrackets .>> pstring "]"

let pColumn = pIdentifier <|> pSBracketColumn
test pColumn "[aaa]"
test pColumn "@a-aa"
test pColumn "123" // error
test pColumn "" // error
test pColumn "[]" // error

let pComma = spaces >>. pstring "," .>> spaces
let pColumnList = sepBy1 pColumn pComma
test pColumnList "aa, @aa , [a a/], a"
test pColumnList " a, a" // error
test pColumnList "" // error

type Expression =
    | Identifier of string
    | ProjectExpression of Expression * string list

let pExpression, pExpressionRef = createParserForwardedToRef()
let pProjectExpression = pstring "project" >>. spaces >>. pExpression .>>. (spaces >>. pColumnList)
pExpressionRef.Value <- pstring "(" >>. ((pProjectExpression |>> ProjectExpression) <|> (pIdentifier |>> Identifier)) .>> pstring ")"

test pProjectExpression "project (シラバス) 専門, 学年, [あ い]]"
test pProjectExpression "project (project (シラバス) 専門, 学年, 場所) 専門, 学年" // 文字化けでerror?
test pProjectExpression "project (project (aa) bb, cc, dd) bb, cc"
test pProjectExpression "project (aa) [bb]]]]]]" // エラーになってほしい
