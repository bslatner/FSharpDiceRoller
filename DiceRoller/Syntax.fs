module Syntax

open System.Text.RegularExpressions

// <expression> ::= <term> | <term> [<addop> <term>]*
// <term>       ::= <factor> [<mulop> factor]*
// <factor>     ::= <dice-roll> | <integer> | (expression)
// <addop>      ::= + | -
// <mulop>      ::= *
// <dice-roll>  ::= d <integer> | <integer> d <integer>
// <integer>    ::= <digit> | <integer> <digit>
// <digit>      ::= 0|1|2|3|4|5|6|7|8|9

type Operator = Plus | Minus | Multiply

type DiceRoll =
    {
        Quantity : int
        Sides : int
    }

type Factor =
    | DiceRoll of DiceRoll
    | Value of int
    | AddOp of LFactor:Factor * Operator:Operator * RFactor:Factor
    | Expression of Term

and Term =
    | Factor of Factor
    | MulOp of LFactor:Factor * Operator:Operator * RFactor:Factor

let private integerRegex = Regex(@"\d+")

let private triml (s : string) = s.TrimStart()

let private after (s : string) i = s.Substring(i)

let (|Int|_|) s =
    let s = triml s
    let m = integerRegex.Match(s)
    match m.Success with
    | true -> Some(int m.Value, after s m.Value.Length)
    | false -> None

let (|D|_|) s =
    let s = triml s
    if s.Length > 0 then
        match s.[0] with
        | 'D' | 'd' -> Some (s.[0], after s 1)
        | _ -> None
    else
        None

let (|DExpr|_|) s =
    match s with
    | D (_,rest) ->
        // single die
        match rest with
        | Int (sides,rest) -> Some ({ Quantity = 1; Sides = sides }, rest)
        | _ -> None
    | _ -> None

let (|Dice|_|) s =
    match s with
    | DExpr result -> Some result
    | Int (quantity,rest) ->
        match rest with
        | DExpr (result,rest) -> Some ({result with Quantity = quantity }, rest)
        | _ -> None
    | _ -> None

let (|AddExp|_|) s =
    let s = triml s
    if s.Length > 0 then
        let c = s.[0]
        let rest = after s 1
        match c with
        | '+' -> Some (Plus,rest)
        | '-' -> Some (Minus,rest)
        | _ -> None
    else
        None

let (|MulExp|_|) s =
    let s = triml s
    if s.Length > 0 then
        let c = s.[0]
        let rest = after s 1
        match c with
        | '*' -> Some (Multiply,rest)
        | _ -> None
    else
        None

let (|LParen|_|) s =
    let s = triml s
    if s.Length > 0 then
        let c = s.[0]
        let rest = after s 1
        match c with
        | '(' -> Some rest
        | _ -> None
    else
        None

let (|RParen|_|) s =
    let s = triml s
    if s.Length > 0 then
        let c = s.[0]
        let rest = after s 1
        match c with
        | ')' -> Some rest
        | _ -> None
    else
        None

let (|SimpleFactor|_|) s =
    match s with
    | Dice (diceRoll,rest) -> Some (DiceRoll diceRoll,rest)
    | Int (i,rest) -> Some (Value i,rest)
    | _ -> None

let rec (|Factor|_|) s =
    match s with
    | LParen (Term (t, RParen rest)) -> Some (Expression t, rest)
    | SimpleFactor (lfactor, AddExp(op, SimpleFactor(rfactor, rest))) -> Some (AddOp (lfactor, op, rfactor), rest)
    | SimpleFactor (factor,rest) -> Some (factor,rest)
    | _ -> None

and (|Term|_|) s =
    match s with
    | Factor (lfactor, MulExp (op, Factor(rfactor, rest))) -> Some (MulOp (lfactor, op, rfactor), rest)
    | Factor (factor, rest) -> Some (Factor factor, rest)
    | _ -> None

let (|DiceExpression|_|) s =
    match s with
    | Term (term,_) -> Some term
    | _ -> failwith "Parser error"
