module day8

open System
open NUnit.Framework
open fsutils.FsUtils

type Instruction = { opcode: string; value: int }
type Program = { instructions: list<Instruction> }
type State = { accumulator: int; ip: int; visited: Set<int> }

type Done = { accumulator: int; error: bool }

let readInput = readLines "../../../input"

let step (p: Program) (s: State) : State*Done option =
    let ip = s.ip
    let visited = s.visited
    let accumulator = s.accumulator
    
    if ip >= (List.length p.instructions) then
        // end of program
        (s, Some({ accumulator = accumulator; error = false }))
    elif Set.contains ip visited then
        // error
        (s, Some({ accumulator = accumulator; error = true }))
    else
        let visited' = Set.add ip visited
        
        let ins = p.instructions.Item ip
        let (ip', accumulator') =
            match ins.opcode with
            | "nop" -> (ip + 1, accumulator)
            | "acc" -> (ip + 1, accumulator + ins.value)
            | "jmp" -> (ip + ins.value, accumulator)
            | _ -> raise (Exception (sprintf "unknown instruction: %s" ins.opcode))
        let s' = { accumulator = accumulator'; ip = ip'; visited = visited' }
        (s', None)
    
let changeIns (p: Program) (at: int) : Program =
    let ins = p.instructions.Item at
    let newIns =
        match ins.opcode with
        | "nop" -> { ins with opcode = "jmp" }
        | "jmp" -> { ins with opcode = "nop" }
        | _ -> ins
    let before = List.take at p.instructions
    let middle = List.singleton newIns
    let after = List.skip (at + 1) p.instructions
    let instructions' = List.concat [ before; middle; after ]
    { p with instructions = instructions' }

let newState : State = { accumulator = 0; ip = 0; visited = Set.empty }

let lineToInstruction (line: string) : Instruction =
    let parts = line.Split(' ')
    { opcode = parts.[0]; value = (int parts.[1]) }

let parseProgram (lines: seq<string>) : Program =
    let instr = List.ofSeq (Seq.map lineToInstruction lines)
    { instructions = instr }
    
let runProgram (p: Program) : Done =
    let rec runRec (p: Program) (s: State) : Done =
        match step p s with
        | (_, Some(d)) -> d         // done, either error or success
        | (s', None) -> runRec p s' // not done, continue running
    runRec p newState
        
let bruteForce (p: Program): Done =
    p.instructions
    |> List.indexed
    |> List.where (fun (_, ins) -> ins.opcode = "nop" || ins.opcode = "jmp")
    |> List.map fst
    |> List.unfold (fun indexes ->
        match indexes with
        | i1 :: rest ->
            match runProgram (changeIns p i1) with
            | d when d.error -> Some(d, rest) // error, continue generation with the remaining indexes
            | d -> Some(d, [])                // success, generate this Done value but then stop
        | [] -> None
        )
    |> List.last                              // the last Done value is the successful one

[<Test>]
let part1 () =
    let p = parseProgram readInput
    let d = runProgram p // acc = 1709
    Assert.That(d.accumulator, Is.EqualTo(1709))
    
[<Test>]
let part2 () =
    let p = parseProgram readInput
    let d = bruteForce p // acc = 1976
    Assert.That(d.accumulator, Is.EqualTo(1976))