namespace fsutils

open System.IO

module FsUtils =

    let readLines (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    // No xor in F#??
    let xor (a: bool) (b: bool) = (a && not b) || (b && not a)
