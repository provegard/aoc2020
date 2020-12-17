module day17_2

open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

type Cube = { active: bool }
type PocketDimension = { cubes: Map<int*int*int*int, Cube> }

type SwitchActiveStrategy = bool -> int -> bool

type CubeFindStrategy = int -> int -> int -> int -> PocketDimension -> list<Cube>

let directions () =
    let offsets = [ -1; 0; 1 ]
    List.allPairs offsets (List.allPairs offsets (List.allPairs offsets offsets))
    |> List.map (fun (a, (b, (c, d))) -> (a, b, c, d))
    |> List.where (fun (a, b, c, d) -> not (a = 0 && b = 0 && c = 0 && d = 0))
    
let getCube (dim: PocketDimension) (pos: int*int*int*int) =
    match dim.cubes.TryFind pos with
    | Some(c) -> c
    | None -> { active = false }

let adjacentCubes (x: int) (y: int) (z: int) (w: int) (dim: PocketDimension) : list<Cube> =
    directions()
    |> List.map (fun (a, b, c, d) -> (a + x, b + y, c + z, d + w))
    |> List.map (getCube dim)
    
let part1SwitchStrategy (active: bool) (activeNeighbors: int) : bool =
    match active with
    | true when activeNeighbors = 2 || activeNeighbors = 3 -> true
    | false when activeNeighbors = 3 -> true
    | _ -> false
    
let emptyPocketDim () : PocketDimension = { cubes = Map.empty }

let withActiveCubeAt (x: int) (y: int) (z: int) (w: int) (dim: PocketDimension) =
    let cubes' = Map.add (x, y, z, w) { active = true } dim.cubes
    { dim with cubes = cubes' }

let toPocketDim (lines: list<string>) : PocketDimension =
    let d = emptyPocketDim()
    let z = 0
    let w = 0
    List.indexed lines
    |> List.fold (fun d' (y, line) ->
        List.indexed ((line.ToCharArray()) |> List.ofArray)
        |> List.fold (fun d'' (x, ch) ->
                if ch = '#' then withActiveCubeAt x y z w d'' else d''
            ) d'
        ) d

let cubeRanges (dim: PocketDimension) : (int*int)*(int*int)*(int*int)*(int*int) =
    let keys = dim.cubes |> Map.toList |> List.map fst
    let (minX, _, _, _) = List.minBy (fun (x, _, _, _) -> x) keys
    let (maxX, _, _, _) = List.maxBy (fun (x, _, _, _) -> x) keys
    let (_, minY, _, _) = List.minBy (fun (_, y, _, _) -> y) keys
    let (_, maxY, _, _) = List.maxBy (fun (_, y, _, _) -> y) keys
    let (_, _, minZ, _) = List.minBy (fun (_, _, z, _) -> z) keys
    let (_, _, maxZ, _) = List.maxBy (fun (_, _, z, _) -> z) keys
    let (_, _, _, minW) = List.minBy (fun (_, _, _, w) -> w) keys
    let (_, _, _, maxW) = List.maxBy (fun (_, _, _, w) -> w) keys
    ((minX, maxX), (minY, maxY), (minZ, maxZ), (minW, maxW))
    
let round (dim: PocketDimension) (switchStrategy: SwitchActiveStrategy) (strategy: CubeFindStrategy) : PocketDimension =
    
    let mapper ((x, y, z, w): int*int*int*int) (cube: Cube) : Cube =
        let neighbors = strategy x y z w dim
        let activeNeighborCount = List.sumBy (fun s -> if s.active then 1 else 0) neighbors
        
        let newActive = switchStrategy cube.active activeNeighborCount
        { active = newActive }
    
    let ((minX, maxX), (minY, maxY), (minZ, maxZ), (minW, maxW)) = cubeRanges dim
    
    let cubes' = seq {
        for x in (minX-1)..(maxX+1) do
            for y in (minY-1)..(maxY+1) do
                for z in (minZ-1)..(maxZ+1) do
                    for w in (minW-1)..(maxW+1) do
                        let pos = (x, y, z, w)
                        let cube = getCube dim pos
                        let cube' = mapper pos cube
                        yield (pos, cube')
                        } |> Map.ofSeq
    
    { dim with cubes = cubes' }
    
let countActive (dim: PocketDimension) : int =
    List.sumBy (fun (_, cube) -> if cube.active then 1 else 0) (dim.cubes |> Map.toList)
    
    
let rounds (dim: PocketDimension) (switchStrategy: SwitchActiveStrategy) (strategy: CubeFindStrategy) (count: int) : int =
    let mutable d = dim
    let mutable r = 0
    while r < count do
        d <- round d switchStrategy strategy
        r <- r + 1
    countActive d
    
    
let testData = @".#.
..#
###"
let testLines = testData.Split('\n') |> List.ofArray

let part1 (lines: list<string>) : int =
    let dim = toPocketDim lines
    rounds dim part1SwitchStrategy adjacentCubes 6

[<Test>]
let Test1 () =
    let result = part1 (readInput())
    Assert.That(result, Is.EqualTo(1520))