module day23

open System
open NUnit.Framework

type Game = { cups: list<int> }

let newGame (cups: list<int>) : Game = { cups = cups }

let move (g: Game) : Game =
    let (currentCup, pickedUp, remaining) =
        match g.cups with
        | cur :: a :: b :: c :: rest -> (cur, [ a; b; c ], cur :: rest)
        | _ -> failwith "unexpected"
    
    let maxCup = List.max remaining
    let minCup = List.min remaining
    
    let rec findDestCupIdx (cs: list<int>) (soughtLabel: int) : int =
        if soughtLabel < minCup then
            List.findIndex (fun c -> c = maxCup) cs
        else
            match List.tryFindIndex (fun c -> c = soughtLabel) cs with
            | Some(idx) -> idx
            | None -> findDestCupIdx cs (soughtLabel - 1)
            
    let destCupIdx = findDestCupIdx remaining (currentCup - 1)
    let (before, after) = List.splitAt (destCupIdx + 1) remaining
    let cups' = (List.tail before) @ pickedUp @ after @ [ currentCup ]
    
    { cups = cups' }
    
let cupsAfterOne (g: Game) : string =
    let idxOfOne = List.findIndex (fun c -> c = 1) g.cups
    let (bef, aft) = List.splitAt idxOfOne g.cups
    let inOrder = (List.tail aft) @ bef
    String.concat "" (inOrder |> List.map (sprintf "%d"))

let play (cups: list<int>) (moves: int) : string =
    let gameAfter =
        [ for i in 1 .. moves -> i ]
        |> List.fold (fun g _ -> move g) (newGame cups)
    cupsAfterOne gameAfter
    
let toCups (s: string) : list<int> =
    s.ToCharArray() |> Array.map (fun ch -> Int32.Parse (sprintf "%c" ch)) |> List.ofArray

[<Test>]
let Test1 () =
    let cups = toCups "389125467"
    let res = play cups 10
    Assert.That(res, Is.EqualTo("92658374"))
    
[<Test>]
let Test1b () =
    let cups = toCups "389125467"
    let res = play cups 100
    Assert.That(res, Is.EqualTo("67384529"))
    
[<Test>]
let Part1 () =
    let cups = toCups "156794823"
    let res = play cups 100
    Assert.That(res, Is.EqualTo("82573496"))