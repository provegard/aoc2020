module day9

open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let testInput = @"35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"
let testLines = testInput.Split('\n') |> List.ofArray

let possibleSums (numbers: list<uint64>) : list<uint64> =
    List.allPairs numbers numbers
    |> List.where (fun (a, b) -> a <> b)
    |> List.map (fun (a, b) -> a + b)
    
let rec findError (numbers: list<uint64>) (preambleLength: int) : uint64 option =
    let preamble = List.take preambleLength numbers
    let rest = List.skip preambleLength numbers
    let sums = possibleSums preamble
    match rest with
    | n :: _ when not (List.contains n sums) -> Some(n)
    | _ :: [] -> None
    | _ -> findError (List.skip 1 numbers) preambleLength 

[<Test>]
let part1 () =
    let numbers = (readInput ()) |> List.map uint64
    let result = findError numbers 25
    Assert.That(result, Is.EqualTo(Some(uint64 373803594)))