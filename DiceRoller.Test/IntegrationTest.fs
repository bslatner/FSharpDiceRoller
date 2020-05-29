module IntegrationTest

//open Expecto
//open Syntax
//open Eval

//let testEval expression expected =
//    let parsed,rest = parseDiceExpression expression
//    let result = eval parsed

//    Expect.equal rest "" ""
//    Expect.equal result expected ""

//[<Tests>]
//let tests = 
//    testList "Integration tests of the sanity variety" [
//        testCase "Simple addition" <| fun _ ->
//            testEval "1+1" 2
//            testEval "1+2" 3
//            testEval "2+2" 4
//            testEval "123+321" 444
//    ]