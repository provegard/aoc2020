module day6

open NUnit.Framework
open fsutils.FsUtils

let readInput = readLines "../../../input"

let toSet (persons: seq<string>) : Set<char> =
    persons
        |> Seq.collect (fun str -> str.ToCharArray())
        |> Seq.fold (fun s ch -> Set.add ch s) Set.empty
    
let toSets (lines: seq<string>) : seq<Set<char>> = readBatches lines |> Seq.map toSet

[<Test>]
let Test1 () =
    let result =
        readInput
        |> toSets
        |> Seq.map (fun s -> s.Count)
        |> Seq.fold (+) 0
    Assert.That(result, Is.EqualTo(6416))
