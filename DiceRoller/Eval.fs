module Eval

open Syntax

let private rnd = System.Random()

let private rollDie sides =
    rnd.Next(sides) + 1

let private roll quantity sides =
    seq {
        for i in 1..quantity -> (rollDie sides)
    } |> Seq.sum

//let rec updateFactor factor =
//    match factor with
//    | DiceRoll diceRoll -> Factor.Value (roll diceRoll.Quantity diceRoll.Sides)
//    | Expression term -> updateTerm term
//    | AddOp (l,op,r) ->
//        let newl = updateFactor l
//        let newr = updateFactor r
//        Factor.AddOp newl,op,newr
//    | _ -> factor

//and updateTerm term =
//    match term with
//    | Term.Factor factor -> Term.Factor (updateFactor factor)
//    | MulOp (l,op,r) ->
//let newl = updateFactor l
//let newr = updateFactor r
//Factor.AddOp newl,op,newr


//let rec rollDice expression
//    match expression with
//    | Term t ->
//        match t with
//        |