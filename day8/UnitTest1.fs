module day8

open System
open NUnit.Framework
open fsutils.FsUtils

type Instruction = { opcode: string; value: int }
type Program = { instructions: list<Instruction> }
type State = { accumulator: int; ip: int; visited: Set<int> }

let readInput = readLines "../../../input"

let testInput = @"nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
let testLines = testInput.Split('\n') |> Seq.ofArray

let step (p: Program) (s: State) : State =
    let ip = s.ip
    let visited = s.visited
    let accumulator = s.accumulator
    
    if Set.contains ip visited then
        raise (Exception (sprintf "already visited: %d - with accumulator %d" ip accumulator))
    
    let visited' = Set.add ip visited
    
    let ins = p.instructions.Item ip
    let (ip', accumulator') =
        match ins.opcode with
        | "nop" -> (ip + 1, accumulator)
        | "acc" -> (ip + 1, accumulator + ins.value)
        | "jmp" -> (ip + ins.value, accumulator)
        | _ -> raise (Exception (sprintf "unknown instruction: %s" ins.opcode))
    let s' = { accumulator = accumulator'; ip = ip'; visited = visited' }
    s'

let newState : State = { accumulator = 0; ip = 0; visited = Set.empty }

let lineToInstruction (line: string) : Instruction =
    let parts = line.Split(' ')
    { opcode = parts.[0]; value = (int parts.[1]) }

let parseProgram (lines: seq<string>) : Program =
    let instr =
        lines
        |> Seq.map lineToInstruction
        |> List.ofSeq
    { instructions = instr }
    
let runProgram (p: Program) : Unit =
    let mutable s = newState
    while true do
        s <- step p s

[<Test>]
let part1 () =
    let p = parseProgram readInput
    runProgram p // acc = 1709
    Assert.Pass()