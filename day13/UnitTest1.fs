module day13

open System
open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq


let parseInput (lines: list<string>) : int*list<int> =
    let est = int (List.head lines)
    let l2 = List.item 1 lines
    let ids = l2.Split(',') |> List.ofArray |> List.choose (fun x ->
        match Int32.TryParse x with
        | true,n -> Some n
        | _ -> None
        )
    (est, ids)

let part1 (est: int) (ids: list<int>) : int =
    let waitTime (id: int) =
        match est % id with
        | 0 -> 0
        | _ ->
            let div = est / id
            let after = id * (div + 1)
            after - est
        
    let x = ids |> List.map (fun id -> (id, waitTime id))
    let (busId, wait) = x |> List.minBy (fun (id, wt) -> wt)
    busId * wait

[<Test>]
let Test1 () =
    let (est, ids) = parseInput (readInput())
    let result = part1 est ids
    
    Assert.That(result, Is.EqualTo(410))