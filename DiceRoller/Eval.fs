module Eval

open Syntax

let private rnd = System.Random()

let private rollDieRandomly sides =
    rnd.Next(sides) + 1

// for testing - test module replaces with constant return value
let mutable rollDie = rollDieRandomly

let private roll quantity sides =
    seq {
        for i in 1..quantity -> (rollDie sides)
    } |> Seq.sum

let eval e =
    let rec evalExpression e =
        match e with
        | Term t -> evalTerm t
        | AddOp (init,others) ->
            let initValue = evalTerm init
            others |> List.fold (fun acc (op,term) -> 
                let termresult = evalTerm term
                match op with 
                | Plus -> acc + termresult
                | Minus -> acc - termresult
                | _ -> failwithf "Evaluation error. Unexpected operator %A. Expected Plus or Minus." op
            ) initValue

    and evalTerm t =
        match t with
        | Factor f -> evalFactor f
        | MulOp (init,others) ->
            let initValue = evalFactor init
            others |> List.fold (fun acc (op,factor) ->
                let factorResult = evalFactor factor
                match op with
                | Multiply -> acc * factorResult
                | _ -> failwithf "Evaluation error. Unexpected operator %A. Expected Multiply." op
            ) initValue

    and evalFactor f =
        match f with
        | DiceRoll d -> roll d.Quantity d.Sides
        | Value v -> v
        | Expression e -> evalExpression e

    evalExpression e