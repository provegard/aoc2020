module day11

open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

type Seat = { occupied: bool }
type SeatingArea = { seats: Map<int*int, Seat>; width: int; height: int }

type SeatFindStrategy = int -> int -> SeatingArea -> list<Seat>

let isValidPos (x: int) (y: int) (sa: SeatingArea) : bool =
    x >= 0 && y >= 0 && x < sa.width && y < sa.height

let directions () =
    let offsets = [ -1; 0; 1 ]
    List.allPairs offsets offsets
    |> List.where (fun (a, b) -> not (a = 0 && b = 0))

let adjacent (x: int) (y: int) (sa: SeatingArea) : list<Seat> =
    directions()
    |> List.map (fun (a, b) -> (a + x, b + y))
    |> List.choose (fun (a, b) -> sa.seats.TryFind (a, b))
    
let visible (x: int) (y: int) (sa: SeatingArea) : list<Seat> =
    
    let rec findSeat (x': int) (y': int) (dx: int) (dy: int) : Seat option =
        if isValidPos x' y' sa then
            match sa.seats.TryFind (x', y') with
            | Some(seat) -> Some(seat)
            | None -> findSeat (x' + dx) (y' + dy) dx dy
        else
            None
    
    directions() |> List.choose (fun (dx, dy) -> findSeat (x + dx) (y + dy) dx dy)

let emptySeatingArea (w: int) (h: int) : SeatingArea = { seats = Map.empty; width = w; height = h }
let withEmptySeatAt (x: int) (y: int) (sa: SeatingArea) =
    let seats' = Map.add (x, y) { occupied = false } sa.seats
    { sa with seats = seats' }

let toSeatingArea (lines: list<string>) : SeatingArea =
    let h = List.length lines
    let w = String.length (List.item 0 lines)
    let sa = emptySeatingArea w h
    List.indexed lines
    |> List.fold (fun sa' (y, line) ->
        List.indexed ((line.ToCharArray()) |> List.ofArray)
        |> List.fold (fun sa'' (x, ch) ->
                if ch = 'L' then
                    withEmptySeatAt x y sa''
                else
                    sa''
            ) sa'
        ) sa
    
let isSeatOccupied (x: int) (y: int) (sa: SeatingArea) : bool =
    match sa.seats.TryFind (x, y) with
    | Some(s) -> s.occupied
    | None -> false
    
let print (sa: SeatingArea) : Unit =
    for y in [ 0 .. (sa.height - 1) ] do
        for x in [ 0 .. (sa.width - 1) ] do
            let ch =
                match sa.seats.TryFind (x, y) with
                | Some(s) when s.occupied -> '#'
                | Some(_) -> 'L'
                | None -> '.'
            printf "%c" ch
        printf "\n"
    printf "\n"
    
let round (sa: SeatingArea) (minOccupiedCount: int) (strategy: SeatFindStrategy) : SeatingArea =
    
    let mapper ((x, y): int*int) (seat: Seat) : Seat =
        let seats = strategy x y sa
        let otherOccupiedCount = List.sumBy (fun s -> if s.occupied then 1 else 0) seats
        
        let newOccupied =
            match (seat.occupied, otherOccupiedCount) with
            | (false, 0) -> true
            | (true, n) when n >= minOccupiedCount -> false
            | (x, _) -> x
        { occupied = newOccupied }
    
    let seats' = Map.map mapper sa.seats
    { sa with seats = seats' }
    
let isDone (prev: SeatingArea option) (current: SeatingArea) : bool =
    Option.exists (fun p -> p.seats = current.seats) prev
    
let countOccupied (sa: SeatingArea) : int =
    List.sumBy (fun (_, seat) -> if seat.occupied then 1 else 0) (sa.seats |> Map.toList)
    
let rounds (seatingArea: SeatingArea) (minOccupiedCount: int) (strategy: SeatFindStrategy) : int =
    let mutable prev: SeatingArea option = None
    let mutable sa = seatingArea
    while not (isDone prev sa) do
        let sa' = round sa minOccupiedCount strategy
        prev <- Some(sa)
        sa <- sa'
    countOccupied sa

[<Test>]
let Test1 () =
    let sa = toSeatingArea (readInput ())
    let result = rounds sa 4 adjacent
    Assert.That(result, Is.EqualTo(2204))
    
[<Test>]
let Test2 () =
    let sa = toSeatingArea (readInput ())
    let result = rounds sa 5 visible
    Assert.That(result, Is.EqualTo(1986))