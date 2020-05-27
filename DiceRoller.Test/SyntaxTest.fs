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
                | _ -> failwithf "Failed to parse input '%s'" s

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

        testCase "AddExp matches operators" <| fun _ ->
            let opTest s expected =
                match s with
                | AddExp (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "'%s' did not parse to %A" s expected

            opTest "+" Plus
            opTest "-" Minus

        testCase "AddExp matches operators with leading whitespace" <| fun _ ->
            let opTest s expected =
                match s with
                | AddExp (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "'%s' did not parse to %A" s expected

            opTest " +" Plus
            opTest "  -" Minus

        testCase "AddExp returns rest of string for operator" <| fun _ ->
            let opTest s expected rest =
                match s with
                | AddExp (value, r) -> 
                    Expect.equal value expected ""
                    Expect.equal r rest ""
                | _ -> failwithf "'%s' did not parse to %A" s expected

            opTest " +abc" Plus "abc"
            opTest "  - hello,world" Minus " hello,world"

        testCase "MulExp matches operators" <| fun _ ->
            let opTest s expected =
                match s with
                | MulExp (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "'%s' did not parse to %A" s expected

            opTest "*" Multiply

        testCase "MulExp matches operators with leading whitespace" <| fun _ ->
            let opTest s expected =
                match s with
                | MulExp (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "'%s' did not parse to %A" s expected

            opTest "   *" Multiply

        testCase "MulExp returns rest of string for operator" <| fun _ ->
            let opTest s expected rest =
                match s with
                | MulExp (value, r) -> 
                    Expect.equal value expected ""
                    Expect.equal r rest ""
                | _ -> failwithf "'%s' did not parse to %A" s expected

            opTest "   *     woot!" Multiply "     woot!"

        testCase "Factor matches dice rolls" <| fun _ ->
            let opTest s expected =
                match s with
                | Factor (f,_) -> 
                    match f with
                    | DiceRoll diceRoll -> Expect.equal diceRoll expected ""
                    | _ -> failwithf "'%s' did not match %A" s expected
                | _ -> failwithf "'%s' did not match %A" s expected

            opTest "d4" { Quantity = 1; Sides = 4 }
            opTest " d 4 " { Quantity = 1; Sides = 4 }
            opTest "2d6" { Quantity = 2; Sides = 6 }
            opTest "2 d6" { Quantity = 2; Sides = 6 }

        testCase "Factor matches integer literals" <| fun _ ->
            let opTest s expected =
                match s with
                | Factor (f,_) -> 
                    match f with
                    | Value v -> Expect.equal v expected ""
                    | _ -> failwithf "'%s' did not match Value %i" s expected
                | _ -> failwithf "'%s' did not match Value %i" s expected

            opTest "1" 1
            opTest " 1" 1
            opTest "23" 23
            opTest "23  " 23

        testCase "Factor matches add and subtract expressions" <| fun _ ->
            let addOp l op r = AddOp (Value l,op,Value r)

            let opTest s expected =
                match s with
                | Factor (f,_) -> 
                    match f with
                    | AddOp (l,op,r) -> Expect.equal (AddOp (l,op,r)) expected ""
                    | _ -> failwithf "'%s' did not match %A" s expected
                | _ -> failwithf "'%s' did not match %A" s expected

            opTest "1+2" (addOp 1 Plus 2)
            opTest "1-2" (addOp 1 Minus 2)
            opTest " 23 + 78 " (addOp 23 Plus 78)
            opTest "987   -    321" (addOp 987 Minus 321)

        testCase "Factor matches dice + literal" <| fun _ ->
            let testFactor s quantity sides operator literal =
                match s with
                | Factor (factor,_) ->
                    match factor with
                    | AddOp (l,op,r) ->
                        match l with
                        | DiceRoll diceRoll -> Expect.equal diceRoll { Quantity = quantity; Sides = sides } ""
                        | _ -> failwithf "Expected DiceRoll but got %A" l 
                        if op <> operator then
                            failwithf "Expected Plus but got %A" op 
                        match r with
                        | Value v -> Expect.equal v literal ""
                        | _ -> failwithf "Expected %i but got %A" literal r 
                    | _ -> failwith "Failed to parse"
                | _ -> failwith "Failed to parse"

            testFactor "d6+4" 1 6 Plus 4
            testFactor "1d6+4" 1 6 Plus 4
            testFactor "2d20+10" 2 20 Plus 10
            testFactor "3 d 100 - 7" 3 100 Minus 7

        testCase "Factor matches literal + dice " <| fun _ ->
            let testFactor s literal operator quantity sides =
                match s with
                | Factor (factor,_) ->
                    match factor with
                    | AddOp (l,op,r) ->
                        match l with
                        | Value v -> Expect.equal v literal ""
                        | _ -> failwithf "Expected %i but got %A" literal l
                        if op <> operator then
                            failwithf "Expected Plus but got %A" op 
                        match r with
                        | DiceRoll diceRoll -> Expect.equal diceRoll { Quantity = quantity; Sides = sides } ""
                        | _ -> failwithf "Expected DiceRoll but got %A" r 
                    | _ -> failwith "Failed to parse"
                | _ -> failwith "Failed to parse"

            testFactor "4+d6" 4 Plus 1 6
            testFactor "4+1d6" 4 Plus 1 6
            testFactor "10+2d20" 10 Plus 2 20
            testFactor "7 - 3 d 100" 7 Minus 3 100

        testCase "Term matches dice rolls" <| fun _ ->
            let opTest s expected =
                match s with
                | Term (t,_) -> 
                    match t with
                    | Term.Factor f ->
                        match f with
                        | DiceRoll diceRoll -> Expect.equal diceRoll expected ""
                        | _ -> failwithf "'%s' did not match %A" s expected
                    | _ -> failwithf "'%s' did not match %A" s expected
                | _ -> failwithf "'%s' did not match %A" s expected


            opTest "d4" { Quantity = 1; Sides = 4 }
            opTest " d 4 " { Quantity = 1; Sides = 4 }
            opTest "2d6" { Quantity = 2; Sides = 6 }
            opTest "2 d6" { Quantity = 2; Sides = 6 }

        testCase "Term matches integer literals" <| fun _ ->
            let opTest s expected =
                match s with
                | Term (t,_) ->
                    match t with
                    | Term.Factor f -> 
                        match f with
                        | Value v -> Expect.equal v expected ""
                        | _ -> failwithf "'%s' did not match Value %i" s expected
                    | _ -> failwithf "'%s' did not match Value %i" s expected
                | _ -> failwithf "'%s' did not match Value %i" s expected

            opTest "1" 1
            opTest " 1" 1
            opTest "23" 23
            opTest "23  " 23

        testCase "Term matches multiplication expressions" <| fun _ ->
            let mulOp l op r = MulOp (Value l,op,Value r)

            let opTest s expected =
                match s with
                | Term (t,_) ->
                    match t with
                    | Term.MulOp (l,op,r) -> Expect.equal (MulOp (l,op,r)) expected ""
                    | _ -> failwithf "'%s' did not match %A" s expected
                | _ -> failwithf "'%s' did not match %A" s expected

            opTest "1*2" (mulOp 1 Multiply 2)
            opTest " 23 * 78 " (mulOp 23 Multiply 78)
            opTest "987   *    321" (mulOp 987 Multiply 321)

        testCase "Term matches dice + literal" <| fun _ ->
            let testTerm s quantity sides operator literal =
                match s with
                | Term (t,_) ->
                    match t with
                    | Term.Factor factor ->
                        match factor with
                        | AddOp (l,op,r) ->
                            match l with
                            | DiceRoll diceRoll -> Expect.equal diceRoll { Quantity = quantity; Sides = sides } ""
                            | _ -> failwithf "Expected DiceRoll but got %A" l 
                            if op <> operator then
                                failwithf "Expected Plus but got %A" op 
                            match r with
                            | Value v -> Expect.equal v literal ""
                            | _ -> failwithf "Expected %i but got %A" literal r 
                        | _ -> failwith "Failed to parse"
                    | _ -> failwith "Failed to parse"
                | _ -> failwith "Failed to parse"

            testTerm "d6+4" 1 6 Plus 4
            testTerm "1d6+4" 1 6 Plus 4
            testTerm "2d20+10" 2 20 Plus 10
            testTerm "3 d 100 - 7" 3 100 Minus 7

        testCase "Term matches literal + dice " <| fun _ ->
            let testTerm s literal operator quantity sides =
                match s with
                | Term (t,_) ->
                    match t with
                    | Term.Factor factor ->
                        match factor with
                        | AddOp (l,op,r) ->
                            match l with
                            | Value v -> Expect.equal v literal ""
                            | _ -> failwithf "Expected %i but got %A" literal l
                            if op <> operator then
                                failwithf "Expected Plus but got %A" op 
                            match r with
                            | DiceRoll diceRoll -> Expect.equal diceRoll { Quantity = quantity; Sides = sides } ""
                            | _ -> failwithf "Expected DiceRoll but got %A" r 
                        | _ -> failwith "Failed to parse"
                    | _ -> failwith "Failed to parse"
                | _ -> failwith "Failed to parse"

            testTerm "4+d6" 4 Plus 1 6
            testTerm "4+1d6" 4 Plus 1 6
            testTerm "10+2d20" 10 Plus 2 20
            testTerm "7 - 3 d 100" 7 Minus 3 100

    ]