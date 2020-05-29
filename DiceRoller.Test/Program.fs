open Expecto

[<EntryPoint>]
let main argv =
    Eval.rollDie <- (fun sides -> sides) // when rolling a die in a test, just return the number of sides
    Tests.runTestsInAssembly defaultConfig argv