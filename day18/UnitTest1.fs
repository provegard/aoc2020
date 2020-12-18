module day18

open System
open System.Text.RegularExpressions
open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let lex (expr: string) : list<string> =
    Regex.Matches(expr, "[0-9]+|\\(|\\)|\\+|\\*| +") |> Seq.map (fun m -> m.Value) |> Seq.where (fun s -> not (String.IsNullOrWhiteSpace(s))) |> List.ofSeq
    
let readParenthesisedExpression (tokens: list<string>) : list<string>*list<string> =
    
    let rec read (collected: list<string>) (tk: list<string>) (balance: int) : list<string>*list<string> =
        if balance = 0 then
            // We collect in reverse, and we need to strip the last closing paren
            (List.rev (List.tail collected), tk)
        else
            match tk with
            | t :: rest when t = "(" -> read (t :: collected) rest (balance + 1)
            | t :: rest when t = ")" -> read (t :: collected) rest (balance - 1)
            | t :: rest -> read (t :: collected) rest balance
            | [] -> failwith "failed to find parenthesized expression end"
    read [] tokens 1
    
let calcExpr (o1: uint64) (op: string) (o2: uint64) : uint64 =
    match op with
    | "*" -> o1 * o2
    | "+" -> o1 + o2
    | _ -> failwith (sprintf "Unknown operator: %s" op)
    
let rec parseExpr (tokens: list<string>) : uint64*list<string> =
    
    let readOperand (tokens: list<string>) : uint64*list<string> =
        match tokens with
        | "(" :: rest ->
            let (innerExpr, rest') = readParenthesisedExpression rest
            let (res, rr) = parseExpr innerExpr
            (res, rr @ rest')
        | n :: rest ->
            match UInt64.TryParse n with
            | true, num -> (num, rest)
            | false, _ -> failwith (sprintf "not a number: '%s'" n)
        | [] -> failwith "no more tokens"
        
    let readOperator (tokens: list<string>) : string*list<string> =
        match tokens with
        | op :: rest -> (op, rest)
        | _ -> failwith "failed to read operator"
        
    let rec calcOne (operand1: uint64) (r1: list<string>) : uint64 =
        let (operator, r2) = readOperator r1
        let (operand2, r3) = readOperand r2
        
        let newOperand1 = calcExpr operand1 operator operand2
    
        match r3 with
        | [] -> newOperand1
        | more -> calcOne newOperand1 more

    let (operand1, r1) = readOperand tokens
    (calcOne operand1 r1, [])
        

let calc (expr: string) : uint64 =
    let tokens = lex expr
    fst (parseExpr tokens)

//[<Test>]
//let Test1 () =
//    Assert.That(calc "1 + 2", Is.EqualTo(3))
//    Assert.That(calc "2 * 3", Is.EqualTo(6))
//    Assert.That(calc "1 + 2 * 3", Is.EqualTo(9))
//    Assert.That(calc "1 + (2 * 3)", Is.EqualTo(7))
//    Assert.That(calc "1 + (2 * 3) + 2", Is.EqualTo(9))
//    Assert.That(calc "6 + ((7 * 9 * 9 + 2) * 2)", Is.EqualTo(1144))
    
[<Test>]
let Test1 () =
    let lines = readInput()
    let sum = lines |> List.map calc |> List.sum
    Assert.That(sum, Is.EqualTo(14208061823964UL))