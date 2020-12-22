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
    
let round1 (p1: Deck) (p2: Deck) : (Deck*Deck) =
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

let rec play1 (p1: Deck) (p2: Deck) : uint64 =
    if hasLost p1 then
        // p2 is the winner
        calcScore p2
    elif hasLost p2 then
        // p1 is the winner
        calcScore p1
    else
        let (p1', p2') = round1 p1 p2
        play1 p1' p2'
        
let deckSize (d: Deck) = List.length d.cards

let rec play2 (p1: Deck) (p2: Deck) (seen: Set<Deck*Deck>) : int*uint64 =
    
    let round2 (p1: Deck) (p2: Deck) : (Deck*Deck) =
        let (c1, p1') = draw p1
        let (c2, p2') = draw p2
        
        let p1HasAtLeastC1CardsLeft = (deckSize p1') >= c1
        let p2HasAtLeastC2CardsLeft = (deckSize p2') >= c2
        
        let winner =
            if p1HasAtLeastC1CardsLeft && p2HasAtLeastC2CardsLeft then
                // recursive sub-game with fewer cards - the 'seen' memory only applies per game, so start empty
                // Copy: the quantity of cards copied is equal to the number on the card they drew to trigger the sub-game
                let p1'' = { cards = List.take c1 p1'.cards }
                let p2'' = { cards = List.take c2 p2'.cards }
                fst (play2 p1'' p2'' Set.empty)
            else
                if c1 > c2 then 1 else 2
        if winner = 1 then
            // p1 won
            let p1'' = putCards p1' c1 c2
            (p1'', p2')
        else
            // p2 won
            let p2'' = putCards p2' c2 c1
            (p1', p2'')
    
    if Set.contains (p1, p2) seen then
        // p1 is the winner
        (1, calcScore p1)
    elif hasLost p1 then
        // p2 is the winner
        (2, calcScore p2)
    elif hasLost p2 then
        // p1 is the winner
        (1, calcScore p1)
    else
        let seen' = Set.add (p1, p2) seen
        let (p1', p2') = round2 p1 p2
        play2 p1' p2' seen'

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
    let result = play1 { cards = testPlayer1 } { cards = testPlayer2 }
    Assert.That(result, Is.EqualTo(306UL))
    
[<Test>]
let Part1 () =
    let (p1, p2) = readPlayers()
    let result = play1 p1 p2
    Assert.That(result, Is.EqualTo(35397UL))
    
[<Test>]
let Test2 () =
    let (_, result) = play2 { cards = testPlayer1 } { cards = testPlayer2 } Set.empty
    Assert.That(result, Is.EqualTo(291UL))
    
[<Test>]
let TestInf () =
    let (_, result) = play2 { cards = [ 43; 19 ] } { cards = [ 2; 29; 14 ] } Set.empty
    Assert.That(result, Is.GreaterThan(0UL))
    
[<Test>]
let Part2 () =
    let (p1, p2) = readPlayers()
    let result = snd (play2 p1 p2 Set.empty)
    Assert.That(result, Is.EqualTo(31120UL))