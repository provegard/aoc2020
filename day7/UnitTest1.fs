module day7

open System
open System.Text.RegularExpressions
open NUnit.Framework
open fsutils.FsUtils

let readInput = readLines "../../../input"

let testInput = @"light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.".Split('\n') |> Seq.ofArray

type BagColor = string
type BagCount = { color: BagColor; count: int }
type Rule = { container: BagCount; contents: seq<BagCount> }

let cleanRegex = Regex("<>")

let rec parseContents (parts: seq<string>) : seq<BagCount> = seq {
    let (bc, rest) =
        match (Seq.tryHead parts) with
        | Some(x) when x = "no" -> ({ color = ""; count = 0 }, Seq.empty)
        | Some(x) -> ({ color = parts |> Seq.skip 1 |> Seq.take 2 |> String.concat " "; count = Int32.Parse(x) }, parts |> Seq.skip 3 )
        | None -> ({ color = ""; count = 0 }, Seq.empty)

    if bc.count > 0 then
        yield bc
        yield! (parseContents rest)
}

let parse (line: string) : Rule =
    // Remove stop words to simplify parsing
    let clean = line.Replace(".", "").Replace(",", "").Replace(" bags", "").Replace(" bag", "").Replace(" contain", "")
    let parts = Seq.ofArray (clean.Split(' '))
    
    let container = parts |> Seq.take 2 |> String.concat " "
    let contents = parts |> Seq.skip 2 |> parseContents
    { container = { color = container; count = 1 }; contents = contents }
    
let isContained (r: Rule) (myBag: BagColor) =
    match Seq.tryFindIndex (fun bc -> bc.color = myBag) r.contents with
    | Some(_) -> true
    | None -> false

let rec containers (rules: seq<Rule>) (myBag: BagColor) : Set<BagColor> =
    let containersForRule (acc: Set<BagColor>) (r: Rule) =
        if (isContained r myBag) then
            Set.union (Set.add r.container.color acc) (containers rules r.container.color)
        else
            acc
    rules |> Seq.fold containersForRule Set.empty

let part1 (lines: seq<string>) (myBag: BagColor) =
    let rules = lines |> Seq.map parse
    let cont = containers rules myBag |> Set.toSeq |> Seq.distinct
    Seq.length cont
    
type Memory = Map<BagColor, int>
let count (rules: seq<Rule>) (myBag: BagColor) : int =
    let rec countInner (bc: BagColor) (mem: Memory) : int*Memory =
//        printf "bc = %s\n" bc
        match mem.TryFind bc with
        | Some(value) -> (value, mem)
        | None ->
            // Find the rule for this color
            let rule = rules |> Seq.find (fun r -> r.container.color = bc)
            
            let immediateContentsCount = Seq.sumBy (fun bc -> bc.count) rule.contents
            
            let (cnt, newMem) =
                rule.contents
                |> Seq.fold (fun (acc: int*Memory) x ->
                        let (prevCount, prevMem) = acc
                        let (cnt', mem') = countInner x.color prevMem
                        let cnt'' = cnt' * x.count
                        (prevCount + cnt'', Map.add x.color cnt'' mem')
                    ) (0, mem)
            let cnt' = cnt + immediateContentsCount
                
//            printf "container = %s, count = %d\n" rule.container.color cnt'
//            printf "-- rule = %A\n" rule
                
            (cnt', newMem)
    let (count, finalMem) = countInner myBag Map.empty
    printf "%A\n" finalMem
    count
    
let part2 (lines: seq<string>) (myBag: BagColor) =
    let rules = lines |> Seq.map parse
    count rules myBag
    
[<Test>]
let test2 () =
    let lines = testInput
    let result = part2 lines "shiny gold"
    Assert.That(result, Is.EqualTo(32))
    
//[<Test>]
//let part1Test () =
//    let lines = readInput
//    let result = part1 lines "shiny gold"
//    Assert.That(result, Is.EqualTo(4))

//[<Test>]
//let part2Test () =
//    let lines = readInput
//    let result = part2 lines "shiny gold"
//    // 85831 is too high
//    Assert.That(result, Is.EqualTo(0))