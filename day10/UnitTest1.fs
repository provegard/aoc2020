module day10

open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let testInput = @"28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"

let testLines = testInput.Split('\n') |> List.ofArray

let countDiffs (adapters: list<int>) : Map<int, int> =
    let maxAdpt = List.max adapters
    let device = maxAdpt + 3
    let adapters' = 0 :: adapters @ [device]
    let sorted: list<int> = List.sort adapters'
    sorted
    |> List.pairwise
    |> List.map (fun (a, b) -> b - a)
    |> List.groupBy (fun n -> n)
    |> List.map (fun (diff, nums) -> (diff, List.length nums))
    |> Map.ofList
    
let part1 (adapters: list<int>) : int =
    let diffs = countDiffs adapters
    (diffs.Item 1) * (diffs.Item 3)
    
[<Test>]
let Test1 () =
    let adapters = readInput() |> List.map int
    Assert.That(part1 adapters, Is.EqualTo(2277))