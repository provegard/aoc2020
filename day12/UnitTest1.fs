module day12_1

open NUnit.Framework
open fsutils.FsUtils
open day12.Common

//type Direction = East | North | West | South
let East = 0
let North = 90
let West = 180
let South = 270

type Ferry = { x: int ; y : int ; direction : int }

let newFerry () : Ferry = { x = 0; y = 0; direction = East }

let moveForward (value: int) (f: Ferry) : Ferry =
    let (dx, dy) =
        match f.direction with
        | n when n = East -> (1, 0)
        | n when n = West -> (-1, 0)
        | n when n = North -> (0, -1)
        | n when n = South -> (0, 1)
        | _ -> failwith (sprintf "Unknown direction %d" f.direction)
    { f with x = f.x + (dx * value); y = f.y + (dy * value) }
    
let changeDirection (value: int) (f: Ferry) : Ferry =
    let mutable newDir = f.direction + value
    if newDir < 0 then
        newDir <- newDir + 360
    { f with direction = newDir % 360 }

let move (f: Ferry) (m: Move) : Ferry =
    match m.action with
    | 'N' -> { f with y = f.y - m.value }
    | 'S' -> { f with y = f.y + m.value }
    | 'E' -> { f with x = f.x + m.value }
    | 'W' -> { f with x = f.x - m.value }
    | 'F' -> moveForward m.value f
    | 'R' -> changeDirection (-m.value) f
    | 'L' -> changeDirection m.value f
    | _ -> failwith (sprintf "Unknown action %c" m.action)

let applyMoves (moves: list<Move>) (f: Ferry) : Ferry =
    List.fold move f moves
    
let distance (f: Ferry) : int =
    manhattan (0, 0) (f.x, f.y)

[<Test>]
let Test1 () =
    let moves = readInput() |> List.map toMove
    let f' = applyMoves moves (newFerry())
    let result = distance f'
    Assert.That(result, Is.EqualTo(1457))