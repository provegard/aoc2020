open System.Linq
open fsutils.FsUtils

type Entry = { min: int; max: int; letter: char; password: string }

let parseLine (line:string) : Entry =
    let parts = line.Split('-', ':', ' ').Where(fun p -> p <> "").ToArray()
    {
        min = parts.[0] |> int
        max = parts.[1] |> int
        letter = parts.[2].[0]
        password = parts.[3]
    }
    
let passwordMatchesPart1 (e: Entry) =
    let actual = e.password.Count(fun ch -> ch = e.letter)
    e.min <= actual && actual <= e.max
    
let passwordMatchesPart2 (e: Entry) =
    let ch1 = e.password.[e.min - 1]
    let ch2 = e.password.[e.max - 1]
    xor (ch1 = e.letter) (ch2 = e.letter)
    
let part (lines: seq<string>) (matcher: Entry -> bool) =
    let matchCount = lines.Select(parseLine).Count(fun e -> matcher e)
    printfn "%d" matchCount

let part1 (lines: seq<string>) = part lines passwordMatchesPart1
let part2 (lines: seq<string>) = part lines passwordMatchesPart2

[<EntryPoint>]
let main argv =
    let lines = readLines "input"
    part1 lines
    part2 lines
    0 // return an integer exit code
