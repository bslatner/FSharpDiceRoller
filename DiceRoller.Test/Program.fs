open Expecto
open Syntax

[<EntryPoint>]
let main argv =

    let parsed = parseDiceExpression "1+2+3+4+5"
    printfn "%A" parsed
    0

    //Tests.runTestsInAssembly defaultConfig argv