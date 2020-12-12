module day12_2

open NUnit.Framework
open fsutils.FsUtils
open System
open day12.Common

type Waypoint = { dx: int ; dy : int }
type Ferry = { x: int ; y: int ; waypoint: Waypoint }

let newFerry () : Ferry = { x = 0; y = 0; waypoint = { dx = 10; dy = -1 } }

let moveForward (value: int) (f: Ferry) : Ferry =
    let w = f.waypoint
    { f with x = f.x + (w.dx * value); y = f.y + (w.dy * value) }
    
let degCos (v: int) : int = Math.Cos(float v * Math.PI / 180.0) |> int
let degSin (v: int) : int = Math.Sin(float v * Math.PI / 180.0) |> int

let rotateCCW (x: int) (y: int) (deg: int): int*int =
    let cos = degCos deg
    let sin = degSin deg
//    let x' = x * cos - y * sin
//    let y' = x * sin + y * cos
    let x' = x * cos + y * sin
    let y' = (-x) * sin + y * cos
    (x', y')
    
let changeDirection (degCW: int) (f: Ferry) : Ferry =
    let w = f.waypoint
    let (dx', dy') = rotateCCW w.dx w.dy (-degCW)
    { f with waypoint = { f.waypoint with dx = dx'; dy = dy' } }

let move (f: Ferry) (m: Move) : Ferry =
    let w = f.waypoint
    match m.action with
    | 'N' -> { f with waypoint = { w with dy = w.dy - m.value } }
    | 'S' -> { f with waypoint = { w with dy = w.dy + m.value } }
    | 'E' -> { f with waypoint = { w with dx = w.dx + m.value } }
    | 'W' -> { f with waypoint = { w with dx = w.dx - m.value } }
    | 'F' -> moveForward m.value f
    | 'R' -> changeDirection m.value f
    | 'L' -> changeDirection (-m.value) f
    | _ -> failwith (sprintf "Unknown action %c" m.action)

let applyMoves (moves: list<Move>) (f: Ferry) : Ferry =
    List.fold move f moves
    
let distance (f: Ferry) : int =
    manhattan (0, 0) (f.x, f.y)

[<Test>]
let Test2 () =
    let moves = (readInput()) |> List.map toMove
    let f' = applyMoves moves (newFerry())
    let result = distance f'
    Assert.That(result, Is.EqualTo(106860))

[<Test>]
let Test_a () =
    let dx = 10
    let dy = -4
    let (dx', dy') = rotateCCW dx dy (-90)
    printf "(%d, %d)\n" dx' dy'
    Assert.That((dx', dy'), Is.EqualTo((4, 10)))
    
