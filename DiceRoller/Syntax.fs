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

and Expression =
    | Term of Term
    | AddOp of Term * ((Operator * Term) list)

and Term =
    | Factor of Factor
    | MulOp of Factor * ((Operator * Factor) list)

and Factor =
    | DiceRoll of Quantity:int * Sides:int
    | Value of int
    | Expression of Expression

let private integerRegex = Regex(@"\d+")

let private trim (s : string) = s.Trim()

let private triml (s : string) = s.TrimStart()

let private after (s : string) i = s.Substring(i)

let (|Int|_|) s =
    let s = triml s
    let m = integerRegex.Match(s)
    match m.Success with
    | true -> Some(int m.Value, after s m.Value.Length)
    | false -> None

let private (|D|_|) s =
    let s = triml s
    if s.Length > 0 then
        match s.[0] with
        | 'D' | 'd' -> Some (s.[0], after s 1)
        | _ -> None
    else
        None

let private (|DE|_|) s =
    match s with
    | D (_,rest) ->
        // single die
        match rest with
        | Int (sides,rest) -> Some ((1,sides), rest)
        | _ -> None
    | _ -> None

let (|DiceE|_|) s =
    match s with
    | DE result -> Some result
    | Int (quantity,DE((_,sides), rest)) -> Some ((quantity,sides), rest)
    | _ -> None

let private (|AddE|_|) s =
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

let private (|MulE|_|) s =
    let s = triml s
    if s.Length > 0 then
        let c = s.[0]
        let rest = after s 1
        match c with
        | '*' -> Some (Multiply,rest)
        | _ -> None
    else
        None

let private (|LParen|_|) s =
    let s = triml s
    if s.Length > 0 then
        let c = s.[0]
        let rest = after s 1
        match c with
        | '(' -> Some rest
        | _ -> None
    else
        None

let private (|RParen|_|) s =
    let s = triml s
    if s.Length > 0 then
        let c = s.[0]
        let rest = after s 1
        match c with
        | ')' -> Some rest
        | _ -> None
    else
        None

let rec (|ExpressionE|_|) s =

    let rec build s lterm =
        match s with
        | AddE(op, TermE(rterm, rest)) ->
            List.append lterm [op,rterm] |> build rest
        | _ -> lterm,s

    let s = triml s
    match s with
    | TermE (lterm, AddE(op, TermE(rterm, rest))) -> 
        let ops,newrest = build rest [op,rterm]
        Some (AddOp (lterm,ops),newrest)
    | TermE (term,rest) -> Some (Expression.Term term,rest)
    | _ -> None

and (|TermE|_|) s =

    let rec build s lfactor =
        match s with
        | MulE(op, FactorE(rfactor, rest)) ->
            List.append lfactor [op,rfactor] |> build rest
        | _ -> lfactor,s

    let s = triml s
    match s with
    | FactorE (lfactor, MulE (op, FactorE(rfactor, rest))) -> 
        let ops,newrest = build rest [op,rfactor]
        Some (MulOp (lfactor,ops),newrest)
    | FactorE (factor, rest) -> Some (Factor factor, rest)
    | _ -> None

and  (|FactorE|_|) s =
    match s with
    | LParen (ExpressionE (e, RParen rest)) -> Some (Expression e, rest)
    | DiceE (diceRoll,rest) -> Some (DiceRoll diceRoll,rest)
    | Int (i,rest) -> Some (Value i,rest)
    | _ -> None


let parseDiceExpression s =
    match s with
    | ExpressionE expression -> expression
    | _ -> failwith "Parser error"
