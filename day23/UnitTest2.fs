module day23_2

open System
open NUnit.Framework

type Cup = { label: int; mutable nextPtr: Cup option; mutable minusOnePtr: Cup option }

type Game = { mutable curPtr: Cup; maxPtr: Cup; onePtr: Cup; size: int }

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
    
let rec findCup (f: Cup -> bool) (c: Cup) : Cup =
    if (f c) then c else (findCup f c.nextPtr.Value)

let buildGame (cupLabels: list<int>) (maxLabel: int) : Game =
    let size = maxLabel // assuming 1 .. maxLabel
    
    let maxInitialLabel = List.max cupLabels //  initialCups |> List.maxBy (fun c -> c.label)
    let initialCups: list<Cup> = cupLabels |> List.map (fun label -> { label = label; nextPtr = None; minusOnePtr = None })
    
    let mutable oneCup: Cup option = None
    
    let mutable prevCup: Cup option = None
    let mutable headCup: Cup option = None
    for i in 1 .. maxLabel do
        let cup =
            if i <= maxInitialLabel then
                List.item (i - 1) initialCups
            else
                { label = i; minusOnePtr = None; nextPtr = None }
        
        // Assign headCup the first time
        if headCup.IsNone then
            headCup <- Some(cup)
            
        // Assign oneCup when we see label 1
        if cup.label = 1 then
            oneCup <- Some(cup)

        cup.minusOnePtr <-
            match prevCup with
            | Some(c) when c.label = (cup.label - 1) -> Some(c)
            | _ -> List.tryFind (fun c -> c.label = cup.label - 1) initialCups
            
        // Connect the previous cup to this one
        Option.iter (fun p -> p.nextPtr <- Some(cup)) prevCup
        
        prevCup <- Some(cup)
        
    // Let the last cup point to the first
    Option.iter (fun c -> c.nextPtr <- headCup) prevCup
    
    let maxCup =
        match prevCup with
        | Some(c) when c.label > maxInitialLabel -> c
        | _ -> findCup (fun c -> c.label = maxInitialLabel) headCup.Value // we'll find it in the first few cups

    // Cup with label 1 has max as prev
    Option.iter (fun c -> c.minusOnePtr <- Some(maxCup)) oneCup
        
    { curPtr = headCup.Value; maxPtr = maxCup; onePtr = oneCup.Value; size = size }
    
let play (g: Game) (moves: int): string =
    for i in 1 .. moves do
        makeMove g
    let collected = cupsAfter (g.onePtr) (g.size - 1)
    String.concat "" (collected |> List.map (fun c -> sprintf "%d" (c.label)))

let play2 (g: Game) (moves: int): uint64 =
    for i in 1 .. moves do
        makeMove g
    let collected = cupsAfter (g.onePtr) 2
    let c1 = List.head collected
    let c2 = List.item 1 collected
    let l1 = uint64 c1.label
    let l2 = uint64 c2.label
    l1 * l2

[<Test>]
let Test1 () =
    let game = buildGame (toCupLabels "389125467") 9
    let res = play game 10
    Assert.That(res, Is.EqualTo("92658374"))
    
[<Test>]
let Test1b () =
    let cups = buildGame (toCupLabels "389125467") 9
    let res = play cups 100
    Assert.That(res, Is.EqualTo("67384529"))
    
[<Test>]
let Part1 () =
    let cups = buildGame (toCupLabels "156794823") 9
    let res = play cups 100
    Assert.That(res, Is.EqualTo("82573496"))
    
[<Test>]
let Test2 () =
    let game = buildGame (toCupLabels "389125467") 1_000_000
    let res = play2 game 10000000
    Assert.That(res, Is.EqualTo(149245887792UL))
    
[<Test>]
let Part2 () =
    let game = buildGame (toCupLabels "156794823") 1_000_000
    let res = play2 game 10000000
    Assert.That(res, Is.EqualTo(11498506800UL))