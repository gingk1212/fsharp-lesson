#r "nuget: FParsec"
open FParsec

run pfloat "1.25"

// let ws = spaces
// let float_ws = pfloat .>> ws
// run float_ws "1.25 "

// 4.2 Parsing a single float
let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25"
test pfloat " 1.25"
test pfloat ""

// 4.3 Parsing a float between brackets
let str s = pstring s
let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"

test floatBetweenBrackets "[1.0]"
test floatBetweenBrackets ""

// 4.4 Abstracting parsers
let betweenStrings s1 s2 p = str s1 >>. p .>> str s2
let floatBetweenBrackets2 = pfloat |> betweenStrings "[" "]"
let floatBetweenDoubleBrackets = pfloat |> betweenStrings "[[" "]]"

test floatBetweenBrackets2 "[1.25]"
test floatBetweenDoubleBrackets "[[1.25]]"

// let between pBegin pEnd p = pBegin >>. p .>> pEnd
// let betweenStrings s1 s2 p = p |> between (str s1) (str s2)

// 4.5 Parsing a list of floats
test (many floatBetweenBrackets) ""
test (many floatBetweenBrackets) "[1.0]"
test (many floatBetweenBrackets) "[2][3][4]"
test (many floatBetweenBrackets) "[1][2.0E][3]"
test (many floatBetweenBrackets) "[2][][4]"
test (many floatBetweenBrackets) "[2][3]a[4]"

let floatList = str "[" >>. sepBy pfloat (str ",") .>> str "]"

test floatList "[]"
test floatList "[1,2,3]"
test floatList "[1,2,3,]"

// 4.6 Handling whitespace
test floatList "[1.0, 2.0]"

let ws = spaces
let str_ws s = pstring s .>> ws
let float_ws = pfloat .>> ws
let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

test numberList "[ 1, 3, 33    , 3 ] "
test numberList @"[ 1 , 
                             2, 3 ] "
test numberList @"[ 1,
                         2; 3]"

let numberListFile = ws >>. numberList .>> eof

test numberListFile " [1, 2, 3] [4]"

// 4.7 Parsing string data
test (many (str "a" <|> str "b")) "abba"

test (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0"

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> ws

test identifier "_"
test identifier "_test1="
test identifier "1"

let stringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))

test stringLiteral "\"abc\""
test stringLiteral "\"abc\\\"def\\\\ghi\""
test stringLiteral "\"abc\\def\""

let stringLiteral2 =
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between (pstring "\"") (pstring "\"")
            (manyStrings (normalCharSnippet <|> escapedChar))

test stringLiteral2 "\"abc\""

let stringLiteral3 =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSnippet escapedChar)

test stringLiteral3 "\"abc\""

// 課題10: カラム名のパーサーを書こう
let pColumn =
    between (pstring "[") (pstring "]")
            (manySatisfy (fun c -> c <> '[' && c <> ']'))

test pColumn "[場所]"

// 課題 10.1 pidentifierを書け
let pidentifier =
    many1Satisfy2 (fun c -> isLetter c) (fun c -> isLetter c || isDigit c)

test pidentifier "a1a1"
test pidentifier "1a1a"

// 課題11: projectのパーサーを書こう
let pProject =
    let pComma = spaces >>. pstring "," .>> spaces
    let pColumnlist = sepBy pColumn pComma
    pstring "project(" >>. pColumnlist .>> pstring ")"

test pProject "project([場所], [学年])"
