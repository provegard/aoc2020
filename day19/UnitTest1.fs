module day19

open NUnit.Framework
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq


type RuleSet = { rules: Map<int, Rule> }
and SubRule = list<int>
and Rule =
    | SimpleMatch of letter: char
    | SubRules of subRules: list<SubRule>
    
let rec matchRule (r: Rule) (rs: RuleSet) (chars: list<char>) : list<char>*list<char> =
    match r with
    | SimpleMatch letter ->
        match chars with
        | x :: rest when x = letter -> ([x], rest)
        | _ -> ([], chars)
    | SubRules subRules ->
        subRules
        |> List.map (fun sr ->

            let (m, u, cont) =
                List.fold (fun acc idx ->
                    let (m, u, cont) = acc
                    if cont then
                        let rule = rs.rules.Item idx
                        let (m', u') = matchRule rule rs u
                        (m', u', not (List.isEmpty m'))
                    else
                        acc
                    ) ([], chars, true) sr
            if cont then
                (m, u)
            else
                ([], chars)

            )
        |> List.maxBy (fun (m, _) -> List.length m)
        
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
        (idx, SimpleMatch(letter))
    | other ->
        let subRules =
            other
            |> separate (fun s -> s = "|")
            |> List.map (List.map int)
        (idx, SubRules(subRules))

let isMatchAgainstRule0 (ruleSet: RuleSet) (str: string) : bool =
    let rule0 = ruleSet.rules.Item 0
    let (_, u) = matchRule rule0 ruleSet (str.ToCharArray() |> List.ofArray)
    List.isEmpty u

[<Test>]
let part1 () =
    let lines = readInput()
    let batches = readBatches lines |> List.ofSeq
    let ruleLines = List.item 0 batches |> List.ofSeq
    let messages = List.item 1 batches |> List.ofSeq
    
    let ruleSet: RuleSet = { rules = ruleLines |> List.map parseRule |> Map.ofList }

    let result =
        messages
        |> List.where (isMatchAgainstRule0 ruleSet)
        |> List.length
        
    Assert.That(result, Is.EqualTo(233))