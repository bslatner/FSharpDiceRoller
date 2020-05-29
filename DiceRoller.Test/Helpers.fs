[<AutoOpen>]
module Helpers

open Syntax

let valuef x = Value x
let valuet x = Term.Factor (valuef x)
let valuee x = Expression.Term (valuet x)

let termifye x = Term.Factor (Factor.Expression x)
let expressifyf x = Expression.Term (Term.Factor x)

let adde x y = AddOp(valuet x,[Plus,valuet y])
let sube x y = AddOp(valuet x,[Minus,valuet y])
let addt x y = Term.Factor (Factor.Expression (adde x y))
let subt x y = Term.Factor (Factor.Expression (sube x y))
let addf x y = Factor.Expression (adde x y)
let subf x y = Factor.Expression (sube x y)
let mult x y = MulOp(valuef x,[Multiply,valuef y])
let mule x y = Expression.Term (mult x y)
let mulf x y = Factor.Expression (mule x y)

let rollf q s = DiceRoll (q,s)
let rollt q s = Term.Factor (rollf q s)
let rolle q s = Expression.Term (rollt q s)