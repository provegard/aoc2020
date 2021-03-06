﻿namespace fsutils

open System.IO
open System

module FsUtils =

    let readLines (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine().Trim()
    }

    // No xor in F#??
    let xor (a: bool) (b: bool) = (a && not b) || (b && not a)

    let private splitLines1 (lines: seq<string>) : seq<string>*seq<string> =
        let firstBatch = lines |> Seq.takeWhile(fun l -> l <> "")
        let rest = lines |> Seq.skipWhile(fun l -> l <> "") |> Seq.skipWhile(fun l -> l = "")
        (firstBatch, rest)
        
    let rec readBatches (lines: seq<string>) : seq<seq<string>> = seq {
        if not (Seq.isEmpty lines) then
            let (firstBatch, rest) = splitLines1 lines
            yield firstBatch
            yield! (readBatches rest)
    }
    
    let manhattan ((x1, y1): int*int) ((x2, y2): int*int) : int =
        Math.Abs(x1 - x2) + Math.Abs(y1 - y2)