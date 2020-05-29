module IntegrationTest

open Expecto
open Syntax
open Eval

let testEval expression expected =
    let parsed,rest = parseDiceExpression expression
    let result = eval parsed

    Expect.equal rest "" ""
    Expect.equal result expected ""

[<Tests>]
let tests = 
    testList "Integration tests of the sanity variety" [
        testCase "Simple addition" <| fun _ ->
            testEval "1+1" 2
            testEval "1+2" 3
            testEval "2+2" 4
            testEval "123+321" 444

        testCase "Simple subtraction" <| fun _ ->
            testEval "1-1" 0
            testEval "2-1" 1
            testEval "4-2" 2
            testEval "444-222" 222

        testCase "Simple multiplication" <| fun _ ->
            testEval "1*2" 2
            testEval "2*3" 6
            testEval "9*8" 72
            testEval "13*17" 221

        testCase "Dice expressions" <| fun _ ->
            testEval "d6+1" 7
            testEval "1+d6" 7
            testEval "1d6+1" 7
            testEval "1+1d6" 7
            testEval "2d4+3" 11
            testEval "3+2d4" 11

        testCase "Complex expressions" <| fun _ ->
            testEval "d6+1*2" 8
            testEval "1+d6*2" 13
            testEval "(1d6+1)*2" 14
            testEval "2*(1+1d6)" 14
            testEval "(2d4+3)+(1d6+1)" 18
    ]