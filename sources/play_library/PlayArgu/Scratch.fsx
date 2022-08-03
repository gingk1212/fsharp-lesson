#r "nuget: Argu"

open Argu

type Arguments =
    | Hoge
    | Foo of path:string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Hoge -> "print Hello World"
            | Foo _ -> "print Bar with arg"

let parser = ArgumentParser.Create<Arguments>(programName="hogehoge")

parser.PrintUsage()

let res = parser.Parse[| "--hoge"; "--foo"; "ikaika" |]

res.Contains Hoge
res.Contains Foo
res.GetResult(Foo)