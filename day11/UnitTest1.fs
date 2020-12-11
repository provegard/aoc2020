module day11

open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let testInput = @"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
let testLines = testInput.Split('\n') |> List.ofArray


type Seat = { occupied: bool }
type SeatingArea = { seats: Map<int*int, Seat>; width: int; height: int }

let adjacent (x: int) (y: int) (sa: SeatingArea) : list<int*int> =
    let offsets = [ -1; 0; 1 ]
    List.allPairs offsets offsets
//    |> List.where (fun (a, b) -> a <> 0 && b <> 0)
    |> List.map (fun (a, b) -> (a + x, b + y))
    |> List.where (fun p -> p <> (x, y))
    |> List.where (fun (a, b) -> a >= 0 && b >= 0 && a < sa.width && b < sa.height)

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

let round (sa: SeatingArea) : SeatingArea =
    
//    print sa
    
    let mapper ((x, y): int*int) (seat: Seat) : Seat =
        let adj = adjacent x y sa
        let isOccupied = seat.occupied
        let adjOccupiedCount = List.sumBy (fun (ax, ay) -> if (isSeatOccupied ax ay sa) then 1 else 0) adj
        
//        if x = 0 && y = 0 then
//            printf "adj = %A\n" adj
//            printf "x=%d y=%d isOccupied=%b adjOccupiedCount=%d\n" x y isOccupied adjOccupiedCount
        
        let newOccupied =
            match (isOccupied, adjOccupiedCount) with
            | (false, 0) -> true
            | (true, n) when n >= 4 -> false
            | (x, _) -> x
        { occupied = newOccupied }
    
    let seats' = Map.map mapper sa.seats
    let ret = { sa with seats = seats' }
    
    // print ret
    
    ret
    
let isDone (prev: SeatingArea option) (current: SeatingArea) : bool =
    Option.exists (fun p -> p.seats = current.seats) prev
    
let countOccupied (sa: SeatingArea) : int =
    List.sumBy (fun (_, seat) -> if seat.occupied then 1 else 0) (sa.seats |> Map.toList)
    
let part1 (seatingArea: SeatingArea) : int =
    let mutable prev: SeatingArea option = None
    let mutable sa = seatingArea
    while not (isDone prev sa) do
        let sa' = round sa
        prev <- Some(sa)
        sa <- sa'
    countOccupied sa

[<Test>]
let Test1 () =
    let sa = toSeatingArea (readInput ())
    let result = part1 sa
    Assert.That(result, Is.EqualTo(2204))