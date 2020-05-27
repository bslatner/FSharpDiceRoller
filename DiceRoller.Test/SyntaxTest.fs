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

        testCase "Int matches returns rest of string for multi-digit" <| fun _ ->
            match "  123abc  " with
            | Int (value, r) -> 
                Expect.equal value 123 ""
                Expect.equal r "abc  " ""
            | _ -> failwith "Failed to parse input"
    ]