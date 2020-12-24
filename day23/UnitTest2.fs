module day23_2

open System
open NUnit.Framework

type Cup = { label: int; mutable nextPtr: Cup option; mutable minusOnePtr: Cup option }

type Game = { mutable curPtr: Cup; maxPtr: Cup; onePtr: Cup; size: int }

let isDetached (c: Cup) : bool =
    false
    
let rec cupsAfter (c: Cup) (count: int) : list<Cup> =
    if count = 0 then
        []
    else
        let next = c.nextPtr.Value
        next :: (cupsAfter next (count - 1))

let rec findDestination (c: Cup) (heldLabels: list<int>) : Cup =
    let m = c.minusOnePtr.Value
    if List.contains m.label heldLabels then
        findDestination m heldLabels
    else
        m

let makeMove (g: Game) : Unit =
    let current = g.curPtr
    let threeAfter = cupsAfter g.curPtr 3
    let heldLabels = threeAfter |> List.map (fun c -> c.label)
    
    let destCup = findDestination g.curPtr heldLabels
    
    // current.nextPtr should point to the cup after the three that were picked up
    // destCup.nextPtr should point to the first of the three that were picked up
    // (last of the three).nextPtr should point to (destCup.nextPtr)
    let lastOfTheTree = List.last threeAfter
    current.nextPtr <- lastOfTheTree.nextPtr
    lastOfTheTree.nextPtr <- destCup.nextPtr
    destCup.nextPtr <- Some(List.head threeAfter)
    
    g.curPtr <- current.nextPtr.Value
    
let toCupLabels (s: string) : list<int> =
    s.ToCharArray() |> Array.map (fun ch -> Int32.Parse (sprintf "%c" ch)) |> List.ofArray

let buildGame (cupLabels: list<int>) : Game =
    let size = List.length cupLabels
    let initialCups: list<Cup> = cupLabels |> List.map (fun label -> { label = label; nextPtr = None; minusOnePtr = None })
    let indexedCups = List.indexed initialCups
    
    let maxCup = initialCups |> List.maxBy (fun c -> c.label)
    
    for (idx, cup) in indexedCups do
        let minusOneLabel = cup.label - 1
        let nextIdx = (idx + 1) % size

        let minusOneCupOpt = List.tryFind (fun c -> c.label = minusOneLabel) initialCups
        cup.minusOnePtr <- (Option.orElse (Some(maxCup)) minusOneCupOpt)

        let (_, nextCup) = List.find (fun (i, c) -> i = nextIdx) indexedCups
        cup.nextPtr <- Some(nextCup)
    
    let headCup = List.head initialCups
    
    let oneCup = List.find (fun c -> c.label = 1) initialCups
        
    { curPtr = headCup; maxPtr = maxCup; onePtr = oneCup; size = size }
    
let play (g: Game) (moves: int): string =
    for i in 1 .. moves do
        makeMove g
    let collected = cupsAfter (g.onePtr) (g.size - 1)
    String.concat "" (collected |> List.map (fun c -> sprintf "%d" (c.label)))
    

[<Test>]
let Test1 () =
    let game = buildGame (toCupLabels "389125467")
    let res = play game 10
    Assert.That(res, Is.EqualTo("92658374"))
    
[<Test>]
let Test1b () =
    let cups = buildGame (toCupLabels "389125467")
    let res = play cups 100
    Assert.That(res, Is.EqualTo("67384529"))
    
[<Test>]
let Part1 () =
    let cups = buildGame (toCupLabels "156794823")
    let res = play cups 100
    Assert.That(res, Is.EqualTo("82573496"))