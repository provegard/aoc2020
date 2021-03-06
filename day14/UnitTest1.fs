module day14

open System
open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

type BitMask = list<char>
type Memory = { data: Map<uint64, uint64>; mask: BitMask }

let applyMask (mask: BitMask) (value: uint64) : uint64 =
    mask
    |> List.rev
    |> List.indexed
    |> List.where (fun (_, ch) -> ch <> 'X')
    |> List.fold (fun v (idx, ch) ->
            let bv = uint64 1 <<< idx
            if ch = '1' then v ||| bv else v &&& ~~~bv
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

let toMask (v: uint64) : BitMask =
    let bch (i: int) =
        let value = uint64 1 <<< i
        if v &&& value <> 0UL then '1' else '0'
    [ for i in 0 .. 35 -> i ] |> List.map bch |> List.rev

let rec gen (chars: list<int*char>) : seq<uint64> = seq {
    match chars with
    | (idx, ch) :: rest ->
        let bv = uint64 1 <<< idx
        let bvs =
            match ch with
            | '0' -> [ 0UL ]
            | '1' -> [ bv ]
            | _   -> [ 0UL ; bv ]
        for bv' in bvs do
            for sub in (gen rest) do
                yield bv' + sub
    | [] ->
        yield 0UL
}

let applyMask2 (mask: BitMask) (value: uint64) : list<uint64> =
    let valueMask = toMask value
    
    let combineChars (vch: char) (mch: char) : char =
        match mch with
        | '0' -> vch
        | '1' -> '1'
        | _   -> 'X'
    
    let applied = List.map2 combineChars valueMask mask |> List.rev |> List.indexed

    gen applied |> List.ofSeq
    
let applyOperation2 (m: Memory) (o: Oper) : Memory =
    match o with
    | SetMask mask -> { m with mask = mask }
    | SetValue (adr, v) ->
        let aa = applyMask2 m.mask adr
        let data' = List.fold (fun d a' -> Map.add a' v d) m.data aa
        { m with data = data' }

let newMemory () : Memory = { data = Map.empty; mask = List.empty }

let run (applyOp: Memory -> Oper -> Memory) (operations: list<Oper>) : Memory =
    List.fold applyOp (newMemory()) operations
    
let sumNonZero (m: Memory) : uint64 =
    Map.toList m.data |> List.sumBy (fun (_, v) -> v)
    
let part (lines: list<string>) (applyOp: Memory -> Oper -> Memory) : uint64 =
    lines |> List.map parseLine |> run applyOp |> sumNonZero
    
let part1 (lines: list<string>): uint64 = part lines applyOperation
let part2 (lines: list<string>): uint64 = part lines applyOperation2
    
[<Test>]
let testApply () =
    let v = 0UL
    let expected = 34359738368UL
    let bm = "1XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".ToCharArray() |> List.ofArray
    let res = applyMask bm v
    Assert.That(res, Is.EqualTo(expected))
    
[<Test>]
let testToMask () =
    let v = 122UL // 1 1 1 1 0 1 0
    let expected = "000000000000000000000000000001111010" //.ToCharArray() |> List.ofArray
    let res = String(toMask v |> Array.ofList)
    Assert.That(res, Is.EqualTo(expected))
    
[<Test>]
let testGen () =
    Assert.Multiple(fun () ->
            let res1 = gen [ (0, '1'); (1, '1'); (2, '1') ] |> List.ofSeq
            Assert.That(res1, Is.EquivalentTo([7]))
        
            let res1 = gen [ (0, '1'); (1, '0'); (2, '1') ] |> List.ofSeq
            Assert.That(res1, Is.EquivalentTo([5]))

            let res1 = gen [ (0, '1'); (1, 'X'); (2, '1') ] |> List.ofSeq
            Assert.That(res1, Is.EquivalentTo([5; 7]))
        )

[<Test>]
let Test1 () =
    let result = part1 (readInput())
    Assert.That(result, Is.EqualTo(18630548206046UL))
    
[<Test>]
let Part2 () =
    let result = part2 (readInput())
    Assert.That(result, Is.EqualTo(4254673508445UL))