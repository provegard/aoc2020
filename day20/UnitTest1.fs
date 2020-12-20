module day20

open System
open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let testInput = @"Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."

let testLines = testInput.Split('\n') |> List.ofArray

type Tile = { id: uint64; lines: list<string> }
type TileSide = string

let parseTile (lines: seq<string>) : Tile =
    let titleLine = Seq.head lines
    let id = uint64 (titleLine.Replace("Tile ", "").Replace(":", ""))
    let tileLines = Seq.tail lines |> List.ofSeq
    { id = id; lines = tileLines }


let reverse (s: string) : string =
    String(s.ToCharArray() |> Array.rev)

// Generates 8 tile sides for a tile,
// assuming the tile can be flipped in any direction
let tileSides (t: Tile) : list<TileSide> =
    let l1 = Seq.head t.lines
    let l2 = Seq.last t.lines
    let l3 = String(t.lines |> List.map (fun l -> l.Chars 0) |> Array.ofList)
    let l4 = String(t.lines |> List.map (fun l -> l.Chars (l.Length - 1)) |> Array.ofList)

    let l1' = reverse l1
    let l2' = reverse l2
    let l3' = reverse l3
    let l4' = reverse l4

    [ l1; l2; l3; l4; l1'; l2'; l3'; l4' ]

let readTiles (lines: list<string>) : list<Tile> =
    let batches = readBatches lines
    batches |> Seq.map parseTile |> List.ofSeq
    
let createTileLookup (tiles: list<Tile>) : Map<string, list<Tile>> =
    tiles
    |> List.fold (fun m tile ->
        tileSides tile
        |> List.fold (fun m' side ->
            match Map.tryFind side m' with
            | Some(tiles) -> Map.add side (tile :: tiles) m'
            | None -> Map.add side [ tile ] m'
            ) m
        ) Map.empty

let findCornerTileIds (tl: Map<string, list<Tile>>) : list<uint64> =
    // We seek tiles with two sides that aren't matched,
    // and since we generate flipped sides also we need
    // to look for tiles with four sides that aren't matched...
    tl
    |> Map.toList
    |> List.where (fun (_, tiles) -> 1 = List.length tiles)
    |> List.map (fun (_, tiles) -> List.head tiles)
    |> List.groupBy (fun t -> t.id)
    |> List.where (fun (_, grouped) -> 4 = List.length grouped)
    |> List.map fst

[<Test>]
let Test1 () =
    let tiles = readTiles (readInput())
    let lookup = createTileLookup tiles
    let ids = findCornerTileIds lookup
    
    let mul = List.fold (*) 1UL ids
    Assert.That(mul, Is.EqualTo(20913499394191UL))