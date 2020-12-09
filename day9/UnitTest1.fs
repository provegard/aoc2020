module day9

open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

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

let part2 (numbers: list<uint64>) (soughtSum: uint64) : uint64 =
    
    let rec find (t: int*int) : uint64 =
        let (start, end') = t
        let length = end' - start + 1
        let subList = numbers |> List.skip start |> List.take length
        let sum = subList |> List.sum
        if sum = soughtSum then
            // done
            (List.min subList) + (List.max subList)
        else if sum > soughtSum then
            find (start + 1, end')
        else
            find (start, end' + 1)
    
    find (0, 1)

[<Test>]
let part1 () =
    let numbers = (readInput ()) |> List.map uint64
    let result = findError numbers 25
    Assert.That(result, Is.EqualTo(Some(uint64 373803594)))
    
[<Test>]
let part2Test () =
    let numbers = (readInput ()) |> List.map uint64
    let result = part2 numbers (uint64 373803594)
    Assert.That(result, Is.EqualTo(uint64 51152360))