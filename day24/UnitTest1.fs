module day24

open System.Text.RegularExpressions
open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let testInput = @"sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"
let testLines = testInput.Split('\n') |> List.ofArray

type Tile = { x: int; y: int; z: int }
type Floor = { black: Set<Tile> }

let dirRegex = Regex("e|se|sw|w|nw|ne")
let lineToDirections (line: string) : list<string> =
    dirRegex.Matches(line) |> Seq.map (fun m -> m.Value) |> List.ofSeq
    
let move (t: Tile) (dir: string) : Tile =
    let (xd, yd, zd) =
        match dir with
        | "nw" -> (0, 1, -1)
        | "ne" -> (1, 0, -1)
        | "e"  -> (1, -1, 0)
        | "se" -> (0, -1, 1)
        | "sw" -> (-1, 0, 1)
        | "w"  -> (-1, 1, 0)
        | _    -> failwith (sprintf "unknown direction: %s" dir)
    { t with x = t.x + xd; y = t.y + yd; z = t.z + zd }
    
let ndirs = [ "nw" ; "ne"; "e" ; "se" ; "sw" ; "w" ]
let neighbors (t: Tile) : list<Tile> =
    ndirs |> List.map (move t)
    
let flipTile (f: Floor) (t: Tile) : Floor =
    if Set.contains t f.black then
        // flip to white
        { black = Set.remove t f.black }
    else
        // flip to black
        { black = Set.add t f.black }

let initialFloor (lines: list<string>) : Floor =
    let floor: Floor = { black = Set.empty }
    let refTile: Tile = { x = 0; y = 0; z = 0 }
    let tiles =
        lines
        |> List.map lineToDirections
        |> List.map (List.fold move refTile)
    List.fold flipTile floor tiles

let part1 (lines: list<string>) : int =
    let flippedFloor = initialFloor lines
    Set.count flippedFloor.black
    
let round (f: Floor) : Floor =
    let allNeighbors = f.black |> Set.toList |> List.collect (neighbors) |> Set.ofList
    let tilesToConsider = Set.union allNeighbors f.black
    
    let newFloor =
        tilesToConsider
        |> Set.toList
        |> List.fold (fun f' t ->
            let tn = neighbors t
            let isBlack = Set.contains t f.black
            let blackNeighbors = tn |> List.map (fun t' -> if Set.contains t' f.black then 1 else 0) |> List.sum
            let doFlip = (isBlack && (blackNeighbors = 0 || blackNeighbors > 2)) ||
                         (not isBlack && blackNeighbors = 2)
            if doFlip then (flipTile f' t) else f'
            ) f
    newFloor

let part2 (lines: list<string>) : int =
    let flippedFloor = initialFloor lines
    let finalFloor = [ 1 .. 100 ] |> List.fold (fun f _ -> round f) flippedFloor
    Set.count finalFloor.black

[<Test>]
let Test1 () =
    let result = part1 testLines
    Assert.That(result, Is.EqualTo(10))
    
[<Test>]
let Part1 () =
    let result = part1 (readInput())
    Assert.That(result, Is.EqualTo(485))
    
[<Test>]
let Test2 () =
    let result = part2 testLines
    Assert.That(result, Is.EqualTo(2208))
    
[<Test>]
let Part2 () =
    let result = part2 (readInput())
    Assert.That(result, Is.EqualTo(3933))