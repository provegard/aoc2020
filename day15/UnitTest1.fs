module day15

open NUnit.Framework

type Game = { numbers: Map<int, list<int>> }

let gen (input: list<int>) : seq<int> = seq {
    let mutable seen: Map<int, list<int>> = Map.empty
    
    let addToSeen (n: int) (r: int) : Unit =
        seen <-
            match Map.tryFind n seen with
            | Some list -> Map.add n (r :: list) seen
            | None -> Map.add n [r] seen
    
    for (i, n) in (input |> List.indexed) do
        yield n
        addToSeen n (i + 1)

    let mutable lastSpoken = List.last input
    let mutable round = 1 + List.length input
    while true do
        match Map.tryFind lastSpoken seen with
        | Some list ->
            match list with
            | [ _ ] ->
                // the last number was new
                lastSpoken <- 0
            | spokenAtRound :: beforeThat :: _ ->
                // the last number has been spoken before
                // List head is round when the number was spoken
                let diff = spokenAtRound - beforeThat
                lastSpoken <- diff
            | _ -> failwith (sprintf "unexpected list %A" list)
            
        | None -> failwith "unexpected 2"

        yield lastSpoken
        addToSeen lastSpoken round
        round <- round + 1
            
}

[<Test>]
let Test1 () =
    let numbers = [ 0;5;4;1;10;14;7 ]
    let s = gen numbers |> Seq.skip 2019 |> Seq.head
    Assert.That(s, Is.EqualTo(203))