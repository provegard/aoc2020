module day5

open NUnit.Framework
open fsutils.FsUtils

let binPart (min: int) (max: int) (ch: char) : int*int =
    let half = min + (max - min + 1) / 2
    let lower = ch = 'F' || ch = 'L'
    if lower then (min, half - 1) else (half, max)

let binPartS (min: int) (max: int) (ss: string) : int*int =
    Array.fold (fun (min', max') ch -> binPart min' max' ch) (min, max) (ss.ToCharArray())
    
let decode (s: string) : int*int =
    let row = fst (binPartS 0 127 (s.Substring(0, 7)))
    let col = fst (binPartS 0 7 (s.Substring(7)))
    (row, col)
    
let seatId (s: string) : int =
    let (row, col) = decode s
    row * 8 + col

[<Test>]
let part1 () =
    let lines = readLines "../../../input"
    let max =
        lines
        |> Seq.map seatId
        |> Seq.max
    Assert.That(max, Is.EqualTo(0))
    
[<Test>]
let setIdTestTest () =
    Assert.That(seatId "FBFBBFFRLR", Is.EqualTo(357))

[<Test>]
let decodeTest () =
    Assert.That(decode "FBFBBFFRLR", Is.EqualTo((44, 5)))

[<Test>]
let binPartTest () =
    Assert.Multiple(fun () ->
        Assert.That(binPart 0 127 'F', Is.EqualTo((0, 63)))
        Assert.That(binPart 0 127 'B', Is.EqualTo((64, 127)))        
        Assert.That(binPart 32 63 'F', Is.EqualTo((32, 47)))
        Assert.That(binPart 32 47 'B', Is.EqualTo((40, 47)))
        Assert.That(binPart 44 45 'F', Is.EqualTo((44, 44)))
        
        Assert.That(binPart 0 7 'R', Is.EqualTo((4, 7)))
        Assert.That(binPart 0 7 'L', Is.EqualTo((0, 3)))
    )

[<Test>]
let binPartSTest () =
    Assert.Multiple(fun () ->
        Assert.That(binPartS 0 127 "", Is.EqualTo((0, 127)))
        Assert.That(binPartS 0 127 "FBFBBFF", Is.EqualTo((44, 44)))
    )
