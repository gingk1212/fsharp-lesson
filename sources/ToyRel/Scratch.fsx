open System.Text.RegularExpressions

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




