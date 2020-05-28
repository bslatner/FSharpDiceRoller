module SyntaxTest

open Expecto
open Syntax

let unwindTermToFactor t =
    match t with
    | Term.Factor f -> f
    | _ -> failwithf "Could not unwind Term %A to a factor" t

let unwindExpressionToFactor e =
    match e with
    | Expression.Term t -> unwindTermToFactor t
    | _ -> failwithf "Could not unwind expression %A to a factor" e

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

        testCase "DiceE matches dice expression with no quantity" <| fun _ ->
            let values = [ 
                "d6",{ Quantity = 1; Sides = 6 }
                "d12",{ Quantity = 1; Sides = 12 }
                "d100",{ Quantity = 1; Sides = 100 }
            ]
            for v in values do
                let s,expected = v
                match s with
                | DiceE (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %A" s expected

        testCase "DiceE matches dice expression with no quantity and leading whitespace" <| fun _ ->
            let values = [ 
                "  d6",{ Quantity = 1; Sides = 6 }
                "   d12",{ Quantity = 1; Sides = 12 }
                " d100",{ Quantity = 1; Sides = 100 }
            ]
            for v in values do
                let s,expected = v
                match s with
                | DiceE (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %A" s expected

        testCase "DiceE matches dice expression with quantity" <| fun _ ->
            let values = [ 
                "1d6",{ Quantity = 1; Sides = 6 }
                "2 d12",{ Quantity = 2; Sides = 12 }
                "3   d100",{ Quantity = 3; Sides = 100 }
            ]
            for v in values do
                let s,expected = v
                match s with
                | DiceE (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %A" s expected

        testCase "DiceE matches dice expression with quantity and leading whitespace" <| fun _ ->
            let values = [ 
                "  1d6",{ Quantity = 1; Sides = 6 }
                "   2 d12",{ Quantity = 2; Sides = 12 }
                " 3  d100",{ Quantity = 3; Sides = 100 }
            ]
            for v in values do
                let s,expected = v
                match s with
                | DiceE (value, _) -> Expect.equal value expected ""
                | _ -> failwithf "%s did not parse to %A" s expected

        testCase "FactorE matches dice rolls" <| fun _ ->
            let opTest s expected =
                match s with
                | FactorE (f,_) -> 
                    match f with
                    | DiceRoll diceRoll -> Expect.equal diceRoll expected ""
                    | _ -> failwithf "'%s' did not match %A" s expected
                | _ -> failwithf "'%s' did not match %A" s expected

            opTest "d4" { Quantity = 1; Sides = 4 }
            opTest " d 4 " { Quantity = 1; Sides = 4 }
            opTest "2d6" { Quantity = 2; Sides = 6 }
            opTest "2 d6" { Quantity = 2; Sides = 6 }

        testCase "FactorE matches integer literals" <| fun _ ->
            let opTest s expected =
                match s with
                | FactorE (f,_) -> 
                    match f with
                    | Value v -> Expect.equal v expected ""
                    | _ -> failwithf "'%s' did not match Value %i" s expected
                | _ -> failwithf "'%s' did not match Value %i" s expected

            opTest "1" 1
            opTest " 1" 1
            opTest "23" 23
            opTest "23  " 23

        testCase "ExpressionE matches add and subtract expressions" <| fun _ ->
            let value v = Term.Factor (Factor.Value v)
            let addOp l op r = AddOp ((value l),op,(value r))

            let opTest s expected =
                match s with
                | ExpressionE (f,_) -> 
                    match f with
                    | AddOp (l,op,r) -> Expect.equal (AddOp (l,op,r)) expected ""
                    | _ -> failwithf "'%s' did not match %A" s expected
                | _ -> failwithf "'%s' did not match %A" s expected

            opTest "1+2" (addOp 1 Plus 2)
            opTest "1-2" (addOp 1 Minus 2)
            opTest " 23 + 78 " (addOp 23 Plus 78)
            opTest "987   -    321" (addOp 987 Minus 321)

        testCase "ExpressionE matches consecutive add and subtract expressions" <| fun _ ->
            let value v = Term.Factor (Factor.Value v)

            match "1+2-3" with
            | ExpressionE (f,_) -> 
                match f with
                | AddOp (l,op,r) -> 
                    Expect.equal l (value 1) ""
                    Expect.equal op Plus ""
                    Expect.equal r (Term.Factor (Factor.Expression(AddOp (value 2,Minus,value 3)))) ""
                | _ -> failwith "'1+2-3 did not parse correctly"
            | _ -> failwith "'1+2-3 did not parse correctly"

        testCase "ExpressionE matches dice + literal" <| fun _ ->
            let testExpression s quantity sides operator literal =
                match s with
                | ExpressionE (e,_) ->
                    match e with
                    | AddOp (l,op,r) ->
                        let l' = unwindTermToFactor l
                        let r' = unwindTermToFactor r
                        match l' with
                        | DiceRoll diceRoll -> Expect.equal diceRoll { Quantity = quantity; Sides = sides } ""
                        | _ -> failwithf "Expected DiceRoll but got %A" l 
                        if op <> operator then
                            failwithf "Expected Plus but got %A" op 
                        match r' with
                        | Value v -> Expect.equal v literal ""
                        | _ -> failwithf "Expected %i but got %A" literal r 
                    | _ -> failwith "Failed to parse"
                | _ -> failwith "Failed to parse"

            testExpression "d6+4" 1 6 Plus 4
            testExpression "1d6+4" 1 6 Plus 4
            testExpression "2d20+10" 2 20 Plus 10
            testExpression "3 d 100 - 7" 3 100 Minus 7

        testCase "ExpressioNE matches literal + dice " <| fun _ ->
            let testExpression s literal operator quantity sides =
                match s with
                | ExpressionE (e,_) ->
                    match e with
                    | AddOp (l,op,r) ->
                        let l' = unwindTermToFactor l
                        let r' = unwindTermToFactor r
                        match l' with
                        | Value v -> Expect.equal v literal ""
                        | _ -> failwithf "Expected %i but got %A" literal l
                        if op <> operator then
                            failwithf "Expected Plus but got %A" op 
                        match r' with
                        | DiceRoll diceRoll -> Expect.equal diceRoll { Quantity = quantity; Sides = sides } ""
                        | _ -> failwithf "Expected DiceRoll but got %A" r 
                    | _ -> failwith "Failed to parse"
                | _ -> failwith "Failed to parse"

            testExpression "4+d6" 4 Plus 1 6
            testExpression "4+1d6" 4 Plus 1 6
            testExpression "10+2d20" 10 Plus 2 20
            testExpression "7 - 3 d 100" 7 Minus 3 100

        testCase "FactorE matches parentheses" <| fun _ ->
            let value v = Term.Factor (Factor.Value v)
            let addOp l op r = AddOp ((value l),op,(value r))

            let opTest s expected =
                match s with
                | FactorE (f,_) ->
                    match f with
                    | Factor.Expression e ->
                        match e with
                        | AddOp (l,op,r) -> Expect.equal (AddOp (l,op,r)) expected ""
                        | _ -> failwithf "'%s' did not match %A" s expected
                    | _ -> failwithf "'%s' did not match %A" s expected
                | _ -> failwithf "'%s' did not match %A" s expected
    
            opTest "(1+2)" (addOp 1 Plus 2)
            opTest "(1-2)" (addOp 1 Minus 2)
            opTest "( 23 + 78 ) " (addOp 23 Plus 78)
            opTest "(987   -    321  )  " (addOp 987 Minus 321)

        testCase "TermE matches multiplication expressions" <| fun _ ->
            let mulOp l op r = MulOp (Value l,op,Value r)

            let opTest s expected =
                match s with
                | TermE (t,_) ->
                    match t with
                    | MulOp (l,op,r) -> Expect.equal (MulOp (l,op,r)) expected ""
                    | _ -> failwithf "'%s' did not match %A" s expected
                | _ -> failwithf "'%s' did not match %A" s expected

            opTest "1*2" (mulOp 1 Multiply 2)
            opTest " 23 * 78 " (mulOp 23 Multiply 78)
            opTest "987   *    321" (mulOp 987 Multiply 321)

        testCase "TermE matches consecutive multiplication expressions" <| fun _ ->

            match "1*2*3" with
            | TermE (t,_) ->
                match t with
                | MulOp (l,op,r) -> 
                    Expect.equal l (Factor.Value 1) ""
                    Expect.equal op Multiply ""
                    Expect.equal r (Factor.Expression (Expression.Term (Term.MulOp (Value 2,Multiply,Value 3)))) ""
                | _ -> failwith "'1*2*3 did not parse correctly"
            | _ -> failwith "'1*2*3 did not parse correctly"

//        //testCase "parseDiceExpression matches various expressions" <| fun _ ->

//        //    let testExpression s expected =
//        //        let exp = parseDiceExpression s
//        //        Expect.equal s exp ""
    ]