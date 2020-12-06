module day6

open NUnit.Framework
open fsutils.FsUtils

let readInput = readLines "../../../input"

let toSet (persons: seq<string>) : Set<char> =
    persons
        |> Seq.collect (fun str -> str.ToCharArray())
        |> Seq.fold (fun s ch -> Set.add ch s) Set.empty
    
let toSets (lines: seq<string>) : seq<Set<char>> = readBatches lines |> Seq.map toSet

let allQ : Set<char> = Set.ofList [ for ascii in 97 .. 122 -> char ascii ]

let toIntersect (persons: seq<string>) : Set<char> =
    persons
        |> Seq.map (fun str -> str.ToCharArray() |> Set.ofArray)
        |> Seq.reduce (fun s1 s2 -> Set.intersect s1 s2)

let toIntersects (lines: seq<string>) : seq<Set<char>> = readBatches lines |> Seq.map toIntersect

[<Test>]
let part1 () =
    let result =
        readInput
        |> toSets
        |> Seq.map (fun s -> s.Count)
        |> Seq.fold (+) 0
    Assert.That(result, Is.EqualTo(6416))


let part2 (lines: seq<string>) =
    lines
        |> toIntersects
        |> Seq.map (fun s -> s.Count)
        |> Seq.fold (+) 0

[<Test>]
let part2Test () =
    let result = part2 readInput
    Assert.That(result, Is.EqualTo(3050))
