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

type Expression =
    | Term of Term
    | AddOp of LTerm:Term * Operator:Operator * RTerm:Term

and Term =
    | Factor of Factor
    | MulOp of LFactor:Factor * Operator:Operator * RFactor:Factor

and Factor =
    | DiceRoll of DiceRoll
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
        | Int (sides,rest) -> Some ({ Quantity = 1; Sides = sides }, rest)
        | _ -> None
    | _ -> None

let (|DiceE|_|) s =
    match s with
    | DE result -> Some result
    | Int (quantity,DE(result, rest)) -> Some ({result with Quantity = quantity }, rest)
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

    let rec buildr s lterm =
        match s with
        | AddE(op, TermE(rterm, rest)) ->
            let rterm',rest' = buildr rest rterm
            Term.Factor (Factor.Expression (AddOp (lterm,op,rterm'))),rest'
        | _ -> lterm,s
            
    let s = triml s
    match s with
    | TermE (lterm, AddE(op, TermE(rterm, rest))) -> 
        let rterm',rest' = buildr rest rterm
        Some (AddOp (lterm, op, rterm'), rest')
    | TermE (term,rest) -> Some (Term term,rest)
    | _ -> None

and (|TermE|_|) s =
    match s with
    | FactorE (lfactor, MulE (op, FactorE(rfactor, rest))) -> Some (MulOp (lfactor, op, rfactor), rest)
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
