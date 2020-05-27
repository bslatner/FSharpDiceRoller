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

type Term =
    | Factor of Factor
    | MulOp of LTerm:Term * Operator:Operator * RTerm:Term

let private integerRegex = Regex(@"\d+")

let private triml (s : string) = s.TrimStart()

let private after (s : string) i = s.Substring(i)

let (|Int|_|) (s : string) =
    let s = triml s
    let m = integerRegex.Match(s)
    match m.Success with
    | true -> Some(int m.Value, after s m.Value.Length)
    | false -> None

let (|D|_|) (s : string) =
    let s = triml s
    if s.Length > 0 then
        match s.[0] with
        | 'D' | 'd' -> Some (s.[0], after s 1)
        | _ -> None
    else
        None

let (|DExpr|_|) (s : string) =
    match s with
    | D (_,rest) ->
        // single die
        match rest with
        | Int (sides,rest) -> Some ({ Quantity = 1; Sides = sides }, rest)
        | _ -> None
    | _ -> None

let (|Dice|_|) (s : string) =
    match s with
    | DExpr result -> Some result
    | Int (quantity,rest) ->
        match rest with
        | DExpr (result,rest) -> Some ({result with Quantity = quantity }, rest)
        | _ -> None
    | _ -> None

let (|AddOp|_|) (s : string) =
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

let (|MulOp|_|) (s : string) =
    let s = triml s
    if s.Length > 0 then
        let c = s.[0]
        let rest = after s 1
        match c with
        | '*' -> Some (Multiply,rest)
        | _ -> None
    else
        None

let (|Factor|_|) (s : string) =
    match s with
    | Dice (diceRoll,rest) -> Some (DiceRoll diceRoll,rest)
    | Int (i,rest) -> Some (Value i,rest)
    | _ -> None
