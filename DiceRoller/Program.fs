open System
open Syntax
open Eval

[<EntryPoint>]
let main argv =
    //let parsed,rest = parseDiceExpression "1+(2-3*4+5)*6"
    //let parsed,rest = parseDiceExpression "2-3*4+5"
    let parsed,rest = parseDiceExpression "2-3*4+5"
    //if not (String.IsNullOrWhiteSpace(rest)) then
    //    printfn "Syntax error at: %s" rest
    //else
    //    let result = eval parsed
    //    printfn "Parsed: %A" parsed
    //    printfn "Result: %i" result
    0 // return an integer exit code
