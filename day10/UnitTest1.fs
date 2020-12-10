module day10

open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let adaptersWithEnds (adapters: list<int>) : list<int> =
    let maxAdpt = List.max adapters
    let device = maxAdpt + 3
    let a = 0 :: adapters @ [device]
    List.sort a

let countDiffs (adapters: list<int>) : Map<int, int> =
    let adapters' = adaptersWithEnds adapters
    adapters'
    |> List.pairwise
    |> List.map (fun (a, b) -> b - a)
    |> List.groupBy (fun n -> n)
    |> List.map (fun (diff, nums) -> (diff, List.length nums))
    |> Map.ofList
    
let comboCount (adapters: list<int>) : uint64 =
    let adapters' = adaptersWithEnds adapters
    let lastIdx = (List.length adapters') - 1
    
    let mutable mem: Map<int, uint64> = Map.empty
    
    let rec count (idx: int) : uint64 =
        if idx = lastIdx then
            uint64 1
        else
            match Map.tryFind idx mem with
            | Some(value) -> value
            | None ->
                let jolts = List.item idx adapters'
                // consider the next three indices
                let res =
                    [ (idx + 1) .. (idx + 3) ]
                    |> List.where (fun i -> (List.tryItem i adapters') |> Option.exists (fun j -> j - jolts <= 3))
                    |> List.map (fun i -> count i)
                    |> List.sum

                mem <- Map.add idx res mem
                res
            
    count 0


let part1 (adapters: list<int>) : int =
    let diffs = countDiffs adapters
    (diffs.Item 1) * (diffs.Item 3)
    
[<Test>]
let Test1 () =
    let adapters = readInput() |> List.map int
    Assert.That(part1 adapters, Is.EqualTo(2277))
    
[<Test>]
let Test2 () =
    let adapters = readInput() |> List.map int
    Assert.That(comboCount adapters, Is.EqualTo(37024595836928UL))