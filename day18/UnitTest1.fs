module day18

open System
open System.Collections
open System.Collections.Generic
open System.Text.RegularExpressions
open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let lex (expr: string) : list<string> =
    Regex.Matches(expr, "[0-9]+|\\(|\\)|\\+|\\*| +") |> Seq.map (fun m -> m.Value) |> Seq.where (fun s -> not (String.IsNullOrWhiteSpace(s))) |> List.ofSeq

type Item =
    | Operand of n: uint64
    | Operator of s: string
    
let firstHasGreaterPrecedenceOrEqualLeftAssociative (first: string) (second: string) : bool =
    true
    
let shouldPopOperator (operatorStack: Stack<string>) (currentOperator: string) : bool =
    match operatorStack.TryPeek() with
    | true, "(" -> false
    | true, o -> firstHasGreaterPrecedenceOrEqualLeftAssociative o currentOperator
    | false, _ -> false
    
let operatorStackTopIs (needle: string) (operatorStack: Stack<string>) =
    match operatorStack.TryPeek() with
    | true, o when o = needle -> true
    | _ -> false

let shuntingYard (tokens: list<string>) : list<Item> =
    let mutable reverseOutputQueue: list<Item> = List.empty
    let operatorStack = Stack<string>()

    let mutable rest = tokens
    while not (List.isEmpty rest) do
        let token = List.head rest
        rest <- List.tail rest

        if token = "(" then
            operatorStack.Push(token)
        elif token = ")" then
            while operatorStack.Count > 0 && not (operatorStackTopIs "(" operatorStack) do
                reverseOutputQueue <- Operator(operatorStack.Pop()) :: reverseOutputQueue
                // TODO If the stack runs out without finding a left parenthesis, then there are mismatched parentheses.
            if operatorStackTopIs "(" operatorStack then
                operatorStack.Pop() |> ignore // discard it
        else
            match UInt64.TryParse token with
            | true, n -> reverseOutputQueue <- Operand(n) :: reverseOutputQueue
            | false, _ ->
                while operatorStack.Count > 0 && (shouldPopOperator operatorStack token) do
                    reverseOutputQueue <- Operator(operatorStack.Pop()) :: reverseOutputQueue
                operatorStack.Push(token)
    while operatorStack.Count > 0 do
        reverseOutputQueue <- Operator(operatorStack.Pop()) :: reverseOutputQueue

    List.rev reverseOutputQueue
    
let applyOperator (o: string) (n1: uint64) (n2: uint64) : uint64 =
    match o with
    | "+" -> n1 + n2
    | "*" -> n1 * n2
    | _ -> failwith (sprintf "Unknown operator '%s'" o)
    
let evaluateRpn (outputQueue: list<Item>) : uint64 =
    let operandStack = Stack<uint64>()
    for item in outputQueue do
        match item with
        | Operand n -> operandStack.Push(n)
        | Operator o ->
            let n1 = operandStack.Pop()
            let n2 = operandStack.Pop()
            operandStack.Push(applyOperator o n1 n2)
    operandStack.Pop()
            
    
let calc (expr: string) : uint64 =
    let tokens = lex expr
    let oq = shuntingYard tokens
    printf "%A\n" oq
    evaluateRpn oq

[<Test>]
let Test1 () =
    Assert.Multiple(fun () ->
        Assert.That(calc "1 + 2", Is.EqualTo(3))
        Assert.That(calc "2 * 3", Is.EqualTo(6))
        Assert.That(calc "1 + 2 * 3", Is.EqualTo(9))
        Assert.That(calc "1 + (2 * 3)", Is.EqualTo(7))
        Assert.That(calc "1 + (2 * 3) + 2", Is.EqualTo(9))
        Assert.That(calc "6 + ((7 * 9 * 9 + 2) * 2)", Is.EqualTo(1144))
        )
    
[<Test>]
let part1 () =
    let lines = readInput()
    let sum = lines |> List.map calc |> List.sum
    Assert.That(sum, Is.EqualTo(14208061823964UL))