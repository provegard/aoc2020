module day12.Common

open fsutils.FsUtils

type Move = { action: char ; value: int }

let readInput () = readLines "../../../input" |> List.ofSeq

let toMove (l: string) : Move =
    let action = l.Chars 0
    let value = int (l.Substring 1)
    { action = action; value = value }