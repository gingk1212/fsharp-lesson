[<EntryPoint>]
let main args =
    if args.Length = 1 && args[0] = "-hello" then
        printfn "Hello World"
    else
        printfn "I don't know"
    0