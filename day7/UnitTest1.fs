module day7

open System
open System.Text.RegularExpressions
open NUnit.Framework
open fsutils.FsUtils

let readInput = readLines "../../../input"

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
    
[<Test>]
let part1Test () =
    let lines = readInput
    let result = part1 lines "shiny gold"
    Assert.That(result, Is.EqualTo(4))