module day14

open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let testInput = @"mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"
let testLines = testInput.Split('\n') |> List.ofArray

type BitMask = list<char>
type Memory = { data: Map<uint64, uint64>; mask: BitMask }

let applyMask (mask: BitMask) (value: uint64) : uint64 =
    mask
    |> List.rev
    |> List.indexed
    |> List.where (fun (_, ch) -> ch <> 'X')
    |> List.fold (fun v (idx, ch) ->
            let bv = uint64 1 <<< idx
            if ch = '1' then
                v ||| bv
            else
                v &&& ~~~bv
        ) value
    
type Oper =
    | SetMask of m: BitMask
    | SetValue of adr: uint64 * value: uint64
    
let parseLine (line: string) : Oper =
    let parts = line.Split([| ' '; '['; ']'; '=' |]) |> List.ofArray |> List.where (fun p -> p <> "")
    match parts with
    | "mask" :: m :: _ -> SetMask(m.ToCharArray() |> List.ofArray)
    | "mem" :: a :: v :: _ -> SetValue (uint64 a, uint64 v)
    | other -> failwith (sprintf "unknown: %A" other)
    
let applyOperation (m: Memory) (o: Oper) : Memory =
    match o with
    | SetMask mask -> { m with mask = mask }
    | SetValue (adr, v) ->
        let v' = applyMask m.mask v
        let data' = Map.add adr v' m.data
        { m with data = data' }

let newMemory () : Memory =
    { data = Map.empty; mask = List.empty }

let run (operations: list<Oper>) : Memory =
    let memory = newMemory()
    List.fold applyOperation memory operations
    
let sumNonZero (m: Memory) : uint64 =
    Map.toList m.data |> List.where (fun (_, v) -> v <> 0UL) |> List.sumBy (fun (_, v) -> v)
    
[<Test>]
let testApply () =
    let v = 0UL
    let expected = 34359738368UL
    let bm = "1XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".ToCharArray() |> List.ofArray
    let res = applyMask bm v
    Assert.That(res, Is.EqualTo(expected))

[<Test>]
let Test1 () =
    let operations = readInput() |> List.map parseLine
    let mem = run operations
    let result = sumNonZero mem
    Assert.That(result, Is.EqualTo(18630548206046UL))