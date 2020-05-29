module Expect

open Expecto
open Syntax
open Eval

let evaluatesTo expression expected =
    let result = eval expression
    Expect.equal result expected ""

let stringEvaluatesTo expression expected =
    let parsed,rest = parseDiceExpression expression
    Expect.isEmpty rest ""
    evaluatesTo parsed expected