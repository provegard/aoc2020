module day19

open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq
let readInput2 () = readLines "../../../input2" |> List.ofSeq


type RuleSet = { rules: Map<int, Rule> }
and SubRule = list<int>
and Rule =
    | SimpleMatch of idx: int * letter: char
    | SubRules of idx: int * subRules: list<SubRule>
    
let isRecursive (ruleIdx: int) (sr: SubRule) : bool =
    List.contains ruleIdx sr
    
let ruleAt (idx: int) (rs: RuleSet) = rs.rules.Item idx

let rec repeat (times: int) (l: list<'a>) : list<'a> =
    match times with
    | 0 -> []
    | n -> l @ (repeat (n - 1) l)

let expandRecursive (recIdx: int) (recursive: list<SubRule>) (nonRecursive: list<SubRule>) (times: int) : SubRule =
    match (recursive, nonRecursive) with
    | ([ r ], [ _ ]) ->
        match r with
        | [ x; n ] when n = recIdx ->
            // 8: 42 | 42 8
            // x one or more times - base case times=0
            repeat (times + 1) [ x ]
        | [ x; n; y ] when n = recIdx ->
            // 11: 42 31 | 42 11 31
            // N times x, N times y, base case times=0
            let n = times + 1
            (repeat n [ x ]) @ (repeat n [ y ])            
        | _ -> failwith "unexpected recursive"
        
    | _ -> failwith "expected 1 recursive and 1 non-recursive"

let toRule (sr: SubRule) : Rule = SubRules(-1, [ sr ])

let rec ruleMatchMaxLength (r: Rule) (rs: RuleSet) : int =
    match r with
    | SimpleMatch _ -> 1
    | SubRules (_, srs) ->
        srs
        |> List.map (fun sr -> sr |> List.map (fun idx -> ruleMatchMaxLength (rs.rules.Item idx) rs) |> List.sum)
        |> List.max
        
type CharRange = list<char>
    
let rec matchRule (r: Rule) (rs: RuleSet) (chars: list<char>) : list<CharRange*CharRange> =
    match r with
    | SimpleMatch (_, letter) ->
        match chars with
        | x :: rest when x = letter -> [ ([x], rest) ]
        | _ -> [ ([], chars) ]
    | SubRules (idx, subRules) ->
        
        let (recursive, nonRecursive) = List.partition (isRecursive idx) subRules
        if not (List.isEmpty recursive) then
            // expand...
            let possibleSubRules: list<SubRule> =
                List.unfold (fun (times) ->
                    let expandedSubRule: SubRule = expandRecursive idx recursive nonRecursive times
                    let maxLen = ruleMatchMaxLength (toRule expandedSubRule) rs
                    let canTheoreticallyMatch = maxLen <= List.length chars
                    if canTheoreticallyMatch then Some(expandedSubRule, times + 1) else None
                    ) 0
                |> List.rev // longest first
            if List.isEmpty possibleSubRules then
                [ ([], chars) ]
            else
                possibleSubRules
                |> List.collect (fun sr ->
                    let rr = toRule sr
                    matchRule rr rs chars
                    )
        else
            
            let rec listVariants (idx0: int) (rest: list<int>) (remaining: CharRange) : list<CharRange*CharRange> =
                let variants =
                    matchRule (ruleAt idx0 rs) rs remaining
                    |> List.where (fun (m1, _) -> not (List.isEmpty m1))
                match rest with
                | nextIdx :: rest' ->
                    variants
                    |> List.collect (fun v ->
                        let (m1, u1) = v
                        let moreVariants = listVariants nextIdx rest' u1
                        moreVariants |> List.map (fun (m2, u2) -> (m1 @ m2, u2))
                        )
                | [] ->
                    variants
            
            subRules
            |> List.collect (fun sr ->
                let idx0 = List.head sr
                let rest = List.tail sr
                listVariants idx0 rest chars
                )
        
let rec separate (separator: 'a -> bool) (l: list<'a>) : list<list<'a>> =
    match List.tryFindIndex separator l with
    | Some(idx) ->
        let (chunk, restInclSep) = List.splitAt idx l
        [chunk] @ (separate separator (List.tail restInclSep))
    | None ->
        [ l ]

let parseRule (line: string) : int*Rule =
    let col = line.IndexOf(':')
    let idx = int (line.Substring(0, col))
    let parts = line.Substring(col + 1).Trim().Split(' ') |> List.ofArray
    match parts with
    | [ p ] when p.StartsWith("\"") ->
        let letter = p.Replace("\"", "").Chars(0)
        (idx, SimpleMatch(idx, letter))
    | other ->
        let subRules =
            other
            |> separate (fun s -> s = "|")
            |> List.map (List.map int)
        (idx, SubRules(idx, subRules))

let isMatchAgainstRule0 (ruleSet: RuleSet) (str: string) : bool =
    let variants =
        matchRule (ruleAt 0 ruleSet) ruleSet (str.ToCharArray() |> List.ofArray)
        |> List.where (fun (m1, u1) -> not (List.isEmpty m1) && (List.isEmpty u1))
    not (List.isEmpty variants)
    
let part (lines: list<string>) =
    let batches = readBatches lines |> List.ofSeq
    let ruleLines = List.item 0 batches |> List.ofSeq
    let messages = List.item 1 batches |> List.ofSeq
    
    let ruleSet: RuleSet = { rules = ruleLines |> List.map parseRule |> Map.ofList }

    let result =
        messages
        |> List.where (isMatchAgainstRule0 ruleSet)
        |> List.length
    result

[<Test>]
let part1 () =
    let lines = readInput()
    let result = part lines
    Assert.That(result, Is.EqualTo(233))
    
[<Test>]
let part2 () =
    let lines = readInput2()
    let result = part lines
    Assert.That(result, Is.EqualTo(396))