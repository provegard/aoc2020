module day16

open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let testInput = @"class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"
let testLines = testInput.Split('\n') |> List.ofArray

type Range = { first: int; last: int }
type Validator = { field: string; ranges: list<Range> }

type Ticket = { values: list<int> }

type TrainContext = { validators: list<Validator>; myTicket: Ticket; nearbyTickets: list<Ticket> }

let parseTicket (line: string) : Ticket =
    let values = line.Split(',') |> List.ofArray |> List.map int
    { values = values }
    
let parseRange (str: string) : Range =
    let parts = str.Split('-')
    { first = (int (parts.[0])); last = (int (parts.[1])) }

let parseValidator (line: string) : Validator =
    let colon = line.IndexOf(':')
    let field = line.Substring(0, colon)
    let ranges = line.Substring(colon + 1).Trim().Split(' ') |> List.ofArray |> List.where (fun s -> s.Contains "-") |> List.map parseRange
    { field = field; ranges = ranges }
    
let parseInput (lines: list<string>) : TrainContext =
    let batches = readBatches lines |> Array.ofSeq
    let validators =
        batches.[0]
        |> List.ofSeq
        |> List.map parseValidator
    let myTicket = batches.[1] |> Seq.skip 1 |> Seq.map parseTicket |> Seq.head
    let nearbyTickets = batches.[2] |> Seq.skip 1 |> Seq.map parseTicket |> List.ofSeq
    { validators = validators; myTicket = myTicket; nearbyTickets = nearbyTickets }
    
let isValid (value: int) (validator: Validator) =
    validator.ranges
    |> List.exists (fun r -> r.first <= value && value <= r.last)
    
let invalidTicketValues (t: Ticket) (validators: list<Validator>) : list<int> =
    let isInvalid (value: int) : bool =
        let validatorsConsideringItValid = validators |> List.where (isValid value)
        0 = List.length validatorsConsideringItValid
        
    t.values |> List.where isInvalid
    
let part1 (lines: list<string>) : int =
    let context = parseInput lines
    let allInvalidValues = context.nearbyTickets |> List.collect (fun t -> invalidTicketValues t context.validators)
    List.sum allInvalidValues

[<Test>]
let Test1 () =
    let result = part1 (readInput ())
    Assert.That(result, Is.EqualTo(28873))