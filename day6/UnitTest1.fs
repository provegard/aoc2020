module day6

open NUnit.Framework
open fsutils.FsUtils

let readInput = readLines "../../../input"

let combine (comb: Set<char> -> Set<char> -> Set<char>) (persons: seq<string>) : Set<char> =
    persons
        |> Seq.map (fun str -> str.ToCharArray() |> Set.ofArray)
        |> Seq.reduce comb

let part (lines: seq<string>) (comb: Set<char> -> Set<char> -> Set<char>) =
    lines
        |> readBatches
        |> Seq.map (combine comb)
        |> Seq.sumBy (fun s -> s.Count)

let part1 (lines: seq<string>) = part lines Set.union
let part2 (lines: seq<string>) = part lines Set.intersect
        
[<Test>]
let part1Test () =
    let result = part1 readInput
    Assert.That(result, Is.EqualTo(6416))

[<Test>]
let part2Test () =
    let result = part2 readInput
    Assert.That(result, Is.EqualTo(3050))
