﻿// Learn more about F# at http://fsharp.org

open System.Linq
open fsutils.FsUtils

type Map = {
    width: int
    height: int
    trees: Set<int*int>
}

type Slide = { x: int; y: int; treesEncountered: int }

let newSlide : Slide = { x = 0; y = 0; treesEncountered = 0 }
let emptyMap : Map = { width = 0; height = 0; trees = Set.empty }

let isSlideDone (m: Map) (s: Slide) : bool = s.y >= m.height

let hasTreeAt (m: Map) (x: int) (y: int) : bool =
    let actualX = x % m.width
    Set.contains (actualX, y) m.trees

let slideStep (m: Map) (s: Slide) (x: int) (y: int) : Slide =
    let x' = s.x + x
    let y' = s.y + y
    let hasTree = hasTreeAt m x' y'
    let diff = if hasTree then 1 else 0
    { x = x'; y = y'; treesEncountered = s.treesEncountered + diff }

let readMap (filePath:string) : Map =
    let lines = readLines filePath
    Seq.fold (fun m (line: string) ->
        let y = m.height
        let treeXs: seq<int> = line.Select(fun ch i -> (ch, i)).Where(fun tup -> (fst tup) = '#').Select(fun tup -> snd tup)
        let newTrees: Set<int*int> = Seq.fold (fun (s: Set<int*int>) (x: int) -> Set.add (x, y) s) m.trees treeXs
        { width = line.Length; height = m.height + 1; trees = newTrees }
    ) emptyMap lines
    
let unfoldGen (x: 'a) : ('a*'a) option = Some(x, x)

let completeSlide (m: Map) (x: int) (y: int) : Slide =
    let doneCheck = isSlideDone m
    newSlide
        |> Seq.unfold (fun s -> unfoldGen (slideStep m s x y))
        |> Seq.takeWhile (fun x -> not (doneCheck x))
        |> Seq.last

let part1 (m: Map) =
    let final = completeSlide m 3 1
    printfn "%d" final.treesEncountered
    
let part2 (m: Map) =
    let angles: List<int*int> = [ (1, 1) ; (3, 1) ; (5, 1) ; (7, 1) ; (1, 2) ]
    
    let doSlide (acc: int) (xy: int*int) =
        let (x, y) = xy
        let s = completeSlide m x y
        acc * s.treesEncountered

    let result: int = List.fold doSlide 1 angles
    
    printfn "%d" result

[<EntryPoint>]
let main argv =
    let m = readMap "input"
    part1 m
    part2 m
    0 // return an integer exit code
