module Syntax

open System.Text.RegularExpressions

// <expression> ::= <term> | <term> <operator> <term>
// <term>       ::= <die-roll> | <integer> | <lparen> <expression> <rparen> | <term> <operator> <term>
// <operator>   ::= + | - | *
// <die-roll>   ::= d <integer> | <integer> d <integer>
// <integer>    ::= <digit> | <integer> <digit>
// <digit>      ::= 0|1|2|3|4|5|6|7|8|9
// <lparen>     ::= (
// <rparen>     ::= )

type Operator = Plus | Minus | Multiply

type DieRoll =
    {
        Quantity : int
        Sides : int
    }

type Term =
    | DieRoll of DieRoll
    | Literal of int
    | Expression of LTerm : Term * Operator : Operator * RTerm : Term

let private integerRegex = Regex(@"\d+")

let private triml (s : string) = s.TrimStart()

let private after (s : string) i = s.Substring(i)

let (|Int|_|) (s : string) =
    let s = triml s
    let m = integerRegex.Match(s)
    match m.Success with
    | true -> Some(int m.Value, (after s m.Value.Length))
    | false -> None
