module day22

open System
open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let testPlayer1 = [ 9 ; 2 ; 6 ; 3 ; 1 ]
let testPlayer2 = [ 5 ; 8 ; 4 ; 7 ; 10 ]

type Deck = { cards: list<int> }

let hasLost (d: Deck) = List.isEmpty d.cards
let draw (d: Deck) : (int*Deck) =
    match d.cards with
    | top :: rest -> (top, { cards = rest })
    | [] -> failwith "invalid draw in empty Deck"
    
let putCards (d: Deck) (c1: int) (c2: int) : Deck =
    { cards = d.cards @ [ c1; c2 ] }
    
let round (p1: Deck) (p2: Deck) : (Deck*Deck) =
    let (c1, p1') = draw p1
    let (c2, p2') = draw p2
    if c1 > c2 then
        // p1 wins
        let p1'' = putCards p1' c1 c2
        (p1'', p2')
    else
        // p2 wins
        let p2'' = putCards p2' c2 c1
        (p1', p2'')
        
let calcScore (d: Deck) : uint64 =
    d.cards
    |> List.rev
    |> List.indexed
    |> List.map (fun (idx, c) -> ((uint64 idx) + 1UL) * (uint64 c))
    |> List.fold (+) 0UL

let rec play (p1: Deck) (p2: Deck) : uint64 =
    if hasLost p1 then
        // p2 is the winner
        calcScore p2
    elif hasLost p2 then
        // p1 is the winner
        calcScore p1
    else
        let (p1', p2') = round p1 p2
        play p1' p2'        

let readPlayers () : (Deck*Deck) =
    let lines = readInput()
    let batches = readBatches lines |> List.ofSeq
    
    let toDeck (lines: seq<string>) : Deck =
        let cards =
            Seq.tail lines
            |> Seq.map int
            |> Seq.toList
        { cards = cards }
    let p1 = toDeck (List.item 0 batches)
    let p2 = toDeck (List.item 1 batches)
    (p1, p2)

[<Test>]
let Test1 () =
    let result = play { cards = testPlayer1 } { cards = testPlayer2 }
    Assert.That(result, Is.EqualTo(306UL))
    
[<Test>]
let Part1 () =
    let (p1, p2) = readPlayers()
    let result = play p1 p2
    Assert.That(result, Is.EqualTo(35397UL))