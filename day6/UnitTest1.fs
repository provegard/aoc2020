module day6

open NUnit.Framework
open fsutils.FsUtils

let readInput = readLines "../../../input"

let doCombine (comb: Set<char> -> Set<char> -> Set<char>) (persons: seq<string>) : Set<char> =
    persons
        |> Seq.map (fun str -> str.ToCharArray() |> Set.ofArray)
        |> Seq.reduce (fun s1 s2 -> comb s1 s2)

let combine (comb: Set<char> -> Set<char> -> Set<char>) (lines: seq<string>) : seq<Set<char>> =
    readBatches lines |> Seq.map (doCombine comb)

let part1 (lines: seq<string>) =
    lines
        |> combine Set.union
        |> Seq.map (fun s -> s.Count)
        |> Seq.fold (+) 0

let part2 (lines: seq<string>) =
    lines
        |> combine Set.intersect
        |> Seq.map (fun s -> s.Count)
        |> Seq.fold (+) 0
        
[<Test>]
let part1Test () =
    let result = part1 readInput
    Assert.That(result, Is.EqualTo(6416))

[<Test>]
let part2Test () =
    let result = part2 readInput
    Assert.That(result, Is.EqualTo(3050))
