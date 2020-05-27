module SyntaxTest

open Expecto
open Syntax

[<Tests>]
let tests = 
    testList "Parser tests" [
        testCase "Int matches single-digit integers" <| fun _ ->
            let values = [ "0",0; "1",1; "2",2; "3",3; "4",4; "5",5; "6",6; "7",7; "8",8; "9",9]
            for v in values do
                let s,expected = v
                match s with
                | Int (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %i" s expected

        testCase "Int matches single-digit integers with leading whitespace" <| fun _ ->
            let values = [ " 0",0; "  1",1; "  2",2; "  3",3; "     4",4; "      5",5; "       6",6; "        7",7; " 8",8; " 9",9]
            for v in values do
                let s,expected = v
                match s with
                | Int (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %i" s expected

        testCase "Int returns rest of string for single-digit integer" <| fun _ ->
            match "  0abc  " with
            | Int (value, r) -> 
                Expect.equal value 0 ""
                Expect.equal r "abc  " ""
            | _ -> failwith "Failed to parse input"

        testCase "Int matches multi-digit integers" <| fun _ ->
            let values = [ "01",1; "12",12; "234",234; "3456",3456]
            for v in values do
                let s,expected = v
                match s with
                | Int (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %i" s expected

        testCase "Int matches multi-digit integers with leading whitespace" <| fun _ ->
            let values = [ " 01",1; "  12",12; "   234",234; "    3456",3456]
            for v in values do
                let s,expected = v
                match s with
                | Int (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %i" s expected

        testCase "Int returns rest of string for multi-digit integer" <| fun _ ->
            match "  123abc  " with
            | Int (value, r) -> 
                Expect.equal value 123 ""
                Expect.equal r "abc  " ""
            | _ -> failwith "Failed to parse input"

        testCase "DExpr matches die expression with no quantity" <| fun _ ->
            let values = [ 
                "d6",{ Quantity = 1; Sides = 6 }
                "d12",{ Quantity = 1; Sides = 12 }
                "d100",{ Quantity = 1; Sides = 100 }
            ]
            for v in values do
                let s,expected = v
                match s with
                | DExpr (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %A" s expected

        testCase "DExpr matches die expression with no quantity and leading whitespace" <| fun _ ->
            let values = [ 
                "  d 6",{ Quantity = 1; Sides = 6 }
                "   d  12",{ Quantity = 1; Sides = 12 }
                " d   100",{ Quantity = 1; Sides = 100 }
            ]
            for v in values do
                let s,expected = v
                match s with
                | DExpr (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %A" s expected

        testCase "DExpr does not matche die expression with quantity" <| fun _ ->
            let values = [ "1d6"; "2d12"; "3d100" ]
            for v in values do
                match v with
                | DExpr _ -> failwithf "%s parse to a DExpr, which was not expected" v
                | _ -> ()

        testCase "DExpr returns rest of string" <| fun _ ->
            let values = [ 
                "  d6+2",{ Quantity = 1; Sides = 6 },"+2"
                "   d12 - 3",{ Quantity = 1; Sides = 12 }," - 3"
                " d100abc",{ Quantity = 1; Sides = 100 },"abc"
            ]
            for v in values do
                let s,expected,rest = v
                match s with 
                | DExpr (value,r) ->
                    Expect.equal value expected ""
                    Expect.equal r rest ""

        testCase "Dice matches dice expression with no quantity" <| fun _ ->
            let values = [ 
                "d6",{ Quantity = 1; Sides = 6 }
                "d12",{ Quantity = 1; Sides = 12 }
                "d100",{ Quantity = 1; Sides = 100 }
            ]
            for v in values do
                let s,expected = v
                match s with
                | Dice (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %A" s expected

        testCase "Dice matches dice expression with no quantity and leading whitespace" <| fun _ ->
            let values = [ 
                "  d6",{ Quantity = 1; Sides = 6 }
                "   d12",{ Quantity = 1; Sides = 12 }
                " d100",{ Quantity = 1; Sides = 100 }
            ]
            for v in values do
                let s,expected = v
                match s with
                | Dice (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %A" s expected

        testCase "Dice matches dice expression with quantity" <| fun _ ->
            let values = [ 
                "1d6",{ Quantity = 1; Sides = 6 }
                "2 d12",{ Quantity = 2; Sides = 12 }
                "3   d100",{ Quantity = 3; Sides = 100 }
            ]
            for v in values do
                let s,expected = v
                match s with
                | Dice (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %A" s expected

        testCase "Dice matches dice expression with quantity and leading whitespace" <| fun _ ->
            let values = [ 
                "  1d6",{ Quantity = 1; Sides = 6 }
                "   2 d12",{ Quantity = 2; Sides = 12 }
                " 3  d100",{ Quantity = 3; Sides = 100 }
            ]
            for v in values do
                let s,expected = v
                match s with
                | Dice (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %A" s expected

        testCase "AddOp matches operators" <| fun _ ->
            let opTest s expected =
                match s with
                | AddOp (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "'%s' did not parse to %A" s expected

            opTest "+" Plus
            opTest "-" Minus

        testCase "AddOp matches operators with leading whitespace" <| fun _ ->
            let opTest s expected =
                match s with
                | AddOp (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "'%s' did not parse to %A" s expected

            opTest " +" Plus
            opTest "  -" Minus

        testCase "AddOp returns rest of string for operator" <| fun _ ->
            let opTest s expected rest =
                match s with
                | AddOp (value, r) -> 
                    Expect.equal value expected ""
                    Expect.equal r rest ""
                | _ -> failwithf "'%s' did not parse to %A" s expected

            opTest " +abc" Plus "abc"
            opTest "  - hello,world" Minus " hello,world"

        testCase "MulOp matches operators" <| fun _ ->
            let opTest s expected =
                match s with
                | MulOp (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "'%s' did not parse to %A" s expected

            opTest "*" Multiply

        testCase "MulOp matches operators with leading whitespace" <| fun _ ->
            let opTest s expected =
                match s with
                | MulOp (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "'%s' did not parse to %A" s expected

            opTest "   *" Multiply

        testCase "MulOp returns rest of string for operator" <| fun _ ->
            let opTest s expected rest =
                match s with
                | MulOp (value, r) -> 
                    Expect.equal value expected ""
                    Expect.equal r rest ""
                | _ -> failwithf "'%s' did not parse to %A" s expected

            opTest "   *     woot!" Multiply "     woot!"

    ]