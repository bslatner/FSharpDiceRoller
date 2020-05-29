open System
open Syntax
open Eval

[<EntryPoint>]
let main argv =
    let expression = String.Join(" ",argv)
    let parsed,rest = parseDiceExpression expression
    if not (String.IsNullOrWhiteSpace(rest)) then
        printfn "Syntax error at: %s" rest
    else
        let result = eval parsed
        #if DEBUG
        printfn "Parsed: %A" parsed
        #endif
        printfn "Result: %i" result
    0 // return an integer exit code
