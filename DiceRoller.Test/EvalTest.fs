﻿module EvalTest

open Expecto
open Syntax

let opify = List.map (fun (op,v) -> op,(valuet v))

[<Tests>]
let tests = 
    testList "Evaluator tests" [
        testCase "Simple addition" <| fun _ ->
            Expect.evaluatesTo (adde 1 1) 2
            Expect.evaluatesTo (adde 1 2) 3
            Expect.evaluatesTo (adde 123 321) 444
            for l in 0..100 do
                for r in 0..100 do
                    Expect.evaluatesTo (adde l r) (l + r)

        testCase "Simple subtraction" <| fun _ ->
            Expect.evaluatesTo (sube 1 1) 0
            Expect.evaluatesTo (sube 2 1) 1
            Expect.evaluatesTo (sube 321 123) 198
            for l in 1..100 do
                for r in l..100 do
                    Expect.evaluatesTo (sube l r) (l - r)

        testCase "Consecutive addition" <| fun _ ->
            Expect.evaluatesTo (AddOp (valuet 1, (opify [Plus,2; Plus,3]))) 6
            Expect.evaluatesTo (AddOp (valuet 1, (opify [Plus,2; Plus,3; Plus,4]))) 10
            Expect.evaluatesTo (AddOp (valuet 1, (opify [Plus,2; Plus,3; Plus,4; Plus,5]))) 15
            Expect.evaluatesTo (AddOp (valuet 1, (opify [Plus,2; Plus,3; Plus,4; Minus,5]))) 5
            Expect.evaluatesTo (AddOp (valuet 1, (opify [Plus,2; Minus,3]))) 0
            Expect.evaluatesTo (AddOp (valuet 1, (opify [Plus,2; Minus,3; Plus,4]))) 4
            Expect.evaluatesTo (AddOp (valuet 1, (opify [Plus,2; Plus,3; Minus,4; Plus,5]))) 7

        testCase "Parentheticals" <| fun _ ->
            Expect.evaluatesTo (AddOp (valuet 10, ([Minus,addt 5 5]))) 0
                   
    ]
