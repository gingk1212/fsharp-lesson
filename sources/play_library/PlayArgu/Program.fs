open System

let args = Environment.GetCommandLineArgs()

if args.Length = 2 && args[1] = "-hello" then
    printfn "Hello World"
else
    printfn "I don't know"