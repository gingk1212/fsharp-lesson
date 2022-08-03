open Argu

[<CliPrefix(CliPrefix.Dash)>]
type Arguments =
    | Hello

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Hello -> "print Hello World"

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Arguments>(programName="PlayArgu")
    let res = parser.Parse(inputs=args, ignoreUnrecognized=true)
    if res.Contains Hello then
        printfn "Hello World"
    else
        printfn "I don't know"

    0