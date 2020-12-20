module day20

open System
open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let seaMonster = @"                  # 
#    ##    ##    ###
 #  #  #  #  #  #   "
let seaMonsterLines = seaMonster.Split('\n') |> List.ofArray

let relativeSeaMonsterPositionsSeq: seq<int*int> = seq {
    for (y, line) in (List.indexed seaMonsterLines) do
        for (x, ch) in (line.ToCharArray() |> Array.indexed) do
            if ch = '#' then
                yield (x, y)
}
let relativeSeaMonsterPositions = relativeSeaMonsterPositionsSeq |> List.ofSeq

type Tile = { id: uint64; lines: list<string> }
type TileSide = string
type TileLookup = Map<string, list<Tile>>
type Grid = { tileMap: Map<int*int, Tile>; gridSize: int }

let parseTile (lines: seq<string>) : Tile =
    let titleLine = Seq.head lines
    let id = uint64 (titleLine.Replace("Tile ", "").Replace(":", ""))
    let tileLines = Seq.tail lines |> List.ofSeq
    { id = id; lines = tileLines }

let reverse (s: string) : string =
    String(s.ToCharArray() |> Array.rev)
    
let rotateRight (t: Tile) : Tile =
    let lineCount = List.length t.lines
    let newLines =
        [ for i in 0 .. (lineCount-1) -> i ]
        |> List.map (fun i ->
            let chars = t.lines |> List.map (fun l -> l.Chars i) |> List.rev |> Array.ofList
            String(chars)
            )
    { t with lines = newLines }
    
let flipHorizontally (t: Tile) : Tile =
    let newLines =
        t.lines
        |> List.map reverse
    { t with lines = newLines }

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
    
let createTileLookup (tiles: list<Tile>) : TileLookup =
    tiles
    |> List.fold (fun m tile ->
        tileSides tile
        |> List.fold (fun m' side ->
            match Map.tryFind side m' with
            | Some(tiles) -> Map.add side (tile :: tiles) m'
            | None -> Map.add side [ tile ] m'
            ) m
        ) Map.empty

let findCornerTileIds (tl: TileLookup) : list<uint64> =
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
    
let tileVariants (t: Tile) : seq<Tile> = seq {
    yield t
    yield (flipHorizontally t)
    let mutable tm = t
    for i in 1 .. 3 do
        tm <- (rotateRight tm)
        yield tm
        yield (flipHorizontally tm)
}
    

let tileTop (t: Tile) : string = List.head t.lines
let tileBottom (t: Tile) : string = List.last t.lines
// bottom-up
let tileLeft (t: Tile) : string =
    let chars = t.lines |> List.map (fun l -> l.Chars 0) |> Array.ofList
    String(chars)
// bottom-up
let tileRight (t: Tile) : string =
    let chars = t.lines |> List.map (fun l -> l.Chars (l.Length - 1)) |> Array.ofList
    String(chars)

let outsideSides (t: Tile) (tl: TileLookup) : list<string> =
    tileSides t
    |> List.where (fun s -> 1 = List.length (tl.Item s))
    
let printTile (t: Tile) : Unit =
    for l in t.lines do
        printf "%s\n" l
        
let toTile (grid: Grid) : Tile =
    let gridSize = grid.gridSize
    let lines =
        seq {
            for y in 0 .. (gridSize-1) do
                let rowTiles = Map.toList grid.tileMap |> List.where (fun ((_, y'), _) -> y' = y) |> List.map snd
                let lineCount = List.length ((List.head rowTiles).lines)
                for l in 0 .. (lineCount - 1) do
                    let allLines = rowTiles |> List.map (fun t -> t.lines.Item l)
                    let combined = String.concat "" allLines
                    yield combined
        } |> List.ofSeq
    { id = 0UL; lines = lines }

let printGrid (grid: Grid) : Unit =
    let gridSize = grid.gridSize
    for y in 0 .. (gridSize-1) do
        let rowTiles = Map.toList grid.tileMap |> List.where (fun ((_, y'), _) -> y' = y) |> List.map snd
        let lineCount = List.length ((List.head rowTiles).lines)
        for l in 0 .. (lineCount - 1) do
            for xt in rowTiles do
                let line = xt.lines.Item l
                printf "%s " line
            printf "\n"
        printf "\n"
        
let withoutBorders (t: Tile) : Tile =
    let lineLength = (List.head t.lines).Length
    let lineCount = List.length t.lines
    let lines' =
        List.tail t.lines
        |> List.take (lineCount - 2)
        |> List.map (fun l -> l.Substring(1, lineLength - 2))
    { t with lines = lines' }

let removeBorders (grid: Grid) : Grid =
    let newMap = Map.toList grid.tileMap |> List.map (fun (pos, t) -> (pos, withoutBorders t)) |> Map.ofList
    { grid with tileMap = newMap }
    
let patternAt (t: Tile) (x: int) (y: int) (w: int) (h: int) : string =
    [ for y' in y .. (y + h - 1) -> y' ]
    |> List.map (fun y' ->
        let line = t.lines.Item y'
        line.Substring(x, w)
        )
    |> List.map (fun s -> s.Replace(".", " "))
    |> String.concat "\n"

let tileCharAt (x: int) (y: int) (t: Tile) : char =
    (t.lines.Item y).Chars x
    
let seaMonsterLocations (t: Tile) : seq<int*int> = seq {
    let monsterHeight = List.length seaMonsterLines
    let monsterWidth = (List.head seaMonsterLines).Length
    let isSeaMonsterAt (x: int) (y: int) : bool =
        
        List.forall (fun (sx, sy) ->
            '#' = tileCharAt (x + sx) (y + sy) t
            ) relativeSeaMonsterPositions
        
    let tileWidth = (List.head t.lines).Length
    for y in 0 .. (List.length t.lines) - monsterHeight do
        for x in 0 .. tileWidth - monsterWidth do
            if isSeaMonsterAt x y then
                yield (x, y)
}

let removeAt (lines: list<string>) (x: int) (y: int) : list<string> =
    List.indexed lines
    |> List.map (fun (idx, line) ->
        if idx = y then
            (line.Substring(0, x)) + " " + (line.Substring(x + 1))
        else
            line
        )

let removeSeaMonster (lines: list<string>) ((x, y): int*int) : list<string> =
    List.fold (fun acc pos ->
        let (px, py) = pos
        removeAt acc (px + x) (py + y)
        ) lines relativeSeaMonsterPositions

let roughness (t: Tile) (locs: list<int*int>) : int =
    let cleanLines = t.lines |> List.map (fun l -> l.Replace(".", " "))
    List.fold (removeSeaMonster) cleanLines locs
    |> List.map (fun l -> l.ToCharArray() |> Array.where (fun ch -> ch = '#') |> Array.length)
    |> List.sum

let arrangeTiles (tiles: list<Tile>) : seq<int> =
    let gridSize = int (Math.Sqrt (float (List.length tiles)))
    
    let lookup = createTileLookup tiles
    let ids = findCornerTileIds lookup
        
    let matchingTiles (t: Tile) : list<Tile> =
        tileSides t
        |> List.collect (fun s ->
            let others = lookup.Item s |> List.where (fun ot -> ot.id <> t.id)
            others
            )
        |> List.distinctBy (fun t -> t.id)
        |> List.collect (fun t -> tileVariants t |> List.ofSeq)
        
    let placeTile (t: Tile) ((x, y): int*int) (grid: Map<int*int, Tile>) : Map<int*int, Tile> =
        Map.add (x, y) t grid
        
    let buildGrid (t0: Tile) : Grid =
        let mutable m = Map.empty
        for y in 0 .. (gridSize-1) do
            for x in 0 .. (gridSize-1) do
                if x = 0 && y = 0 then
                    m <- placeTile t0 (x, y) m
                elif x > 0 then
                    let toTheLeft = Map.find (x - 1, y) m
                    let matches =
                        matchingTiles toTheLeft
                        |> List.where (fun t' -> (tileRight toTheLeft) = (tileLeft t'))
                        
                    match matches with
                    | [ t' ] ->
                        m <- placeTile t' (x, y) m
                    | [] -> failwith (sprintf "failed to find a tile to place to the right of %d" toTheLeft.id)
                    | ms ->
                        let ids' = ms |> List.map (fun t -> t.id)
                        failwith (sprintf "found multiple tiles (%A) to place to the right of %d" ids' toTheLeft.id)
                else
                    let above = Map.find (x, y - 1) m
                    let matches =
                        matchingTiles above
                        |> List.where (fun t' -> (tileBottom above) = (tileTop t'))
                        
                    match matches with
                    | [ t' ] ->
                        m <- placeTile t' (x, y) m
                    | [] -> failwith (sprintf "failed to find a tile to place below %d" above.id)
                    | ms ->
                        let ids' = ms |> List.map (fun t -> t.id)
                        failwith (sprintf "found multiple tiles (%A) to place below of %d" ids' above.id)
        { tileMap = m; gridSize = gridSize }

    seq {
        for cornerIdx in [ List.head ids ] do
            let cornerTile = List.find (fun t -> t.id = cornerIdx) tiles
            
            let os = outsideSides cornerTile lookup
            let possibleTopLeftCornerVariants =
                tileVariants cornerTile
                |> Seq.where (fun v ->
                    let vTop = tileTop v
                    let vLeft = tileLeft v
                    (List.contains vTop os) && (List.contains vLeft os)
                    )
                |> List.ofSeq
            
            for v in [ List.head possibleTopLeftCornerVariants ] do
                let grid = buildGrid v
                
                let grid' = removeBorders grid
                let newTile = toTile grid'
                
                for tv in (tileVariants newTile) do
                    let locs = seaMonsterLocations tv |> List.ofSeq
                    let cnt = List.length locs
                    if cnt > 0 then
                        let rough = roughness tv locs
                        yield rough
    }

[<Test>]
let Test1 () =
    let tiles = readTiles (readInput())
    let lookup = createTileLookup tiles
    let ids = findCornerTileIds lookup
    
    let mul = List.fold (*) 1UL ids
    Assert.That(mul, Is.EqualTo(20913499394191UL))
    
[<Test>]
let Test2 () =
    let tiles = readTiles (readInput())
    let result = arrangeTiles tiles |> Seq.head
    Assert.That(result, Is.EqualTo(2209))
