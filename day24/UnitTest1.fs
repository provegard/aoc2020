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

//type Color = Black | White
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
    
let flipTile (f: Floor) (t: Tile) : Floor =
    if Set.contains t f.black then
        // flip to white
        { black = Set.remove t f.black }
    else
        // flip to black
        { black = Set.add t f.black }

let part1 (lines: list<string>) : int =
    let floor: Floor = { black = Set.empty }
    let refTile: Tile = { x = 0; y = 0; z = 0 }
    let tiles =
        lines
        |> List.map lineToDirections
        |> List.map (List.fold move refTile)
    let flippedFloor = List.fold flipTile floor tiles
    Set.count flippedFloor.black

[<Test>]
let Test1 () =
    let result = part1 testLines
    Assert.That(result, Is.EqualTo(10))
    
[<Test>]
let Part1 () =
    let result = part1 (readInput())
    Assert.That(result, Is.EqualTo(485))