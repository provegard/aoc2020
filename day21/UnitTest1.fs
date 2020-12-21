module day21

open NUnit.Framework
open System.Text.RegularExpressions
open fsutils.FsUtils

let readInput () = readLines "../../../input" |> List.ofSeq

let testInput = @"mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"
let testLines = testInput.Split('\n') |> List.ofArray

type Food = { ingredients: list<string>; allergens: list<string> }

let stringParts (s: string) : list<string> =
    s.Trim().Split(' ') |> List.ofArray |> List.map (fun s -> s.Replace(",", "").Trim()) |> List.where (fun s -> s <> "")

let foodRegex = Regex("^(?<ingr>.*?)( \\(contains (?<all>.*)\\))?$")
let parseFood (line: string) : Food =
    let m = foodRegex.Match(line)
    let ingredientsStr = m.Groups.["ingr"].Value
    let allergensStr = m.Groups.["all"].Value
    
    let ingredients = stringParts ingredientsStr
    let allergens = stringParts allergensStr
    
    { ingredients = ingredients; allergens = allergens }
    
let getIngredientsPerAllergen (foods: list<Food>) (allAllergens: list<string>) : Map<string, Set<string>> =
    allAllergens
    |> List.map (fun a ->
        let ingredients =
            foods
            |> List.where (fun f -> List.contains a f.allergens)
            |> List.map (fun f -> f.ingredients)
            |> List.map Set.ofList
            |> Set.intersectMany
        
        (a, ingredients)
        )
    |> Map.ofList

let findInertIngredients (foods: list<Food>): list<string> =
    let allAllergens = foods |> List.collect (fun f -> f.allergens) |> List.distinct
    let allIngredients = foods |> List.collect (fun f -> f.ingredients) |> List.distinct

    let ingredientsPerAllergen = getIngredientsPerAllergen foods allAllergens
    let candidateIngredients = Set.unionMany (Map.toList ingredientsPerAllergen |> List.map snd)
        
    allIngredients
    |> List.where (fun i -> not (Set.contains i candidateIngredients))
    
let part1 (lines: list<string>) : int =
    let foods = lines |> List.map parseFood
    let inertIngredients = findInertIngredients foods
        
    let countInFood =
        inertIngredients
        |> List.map (fun i -> foods |> List.where (fun f -> List.contains i f.ingredients) |> List.length)
        |> List.sum
        
    countInFood
    
// copied from day 16
let narrowAllergenCandidates (candidates: list<string*list<string>>) : list<string*string> =
    
    let rec narrow (rest: list<string*list<string>>) : list<string*string> =
        if List.isEmpty rest then
            []
        else
            let (ready, needWork) = rest |> List.partition (fun (_, cs) -> 1 = List.length cs)
            let readyList = ready |> List.map (fun (fn, cs) -> (fn, List.head cs))
            let readySet = readyList |> List.map snd |> Set.ofList
            let stripped = needWork |> List.map (fun (fn, cs) ->
                let cs' = cs |> List.where (fun c -> not (Set.contains c readySet))
                (fn, cs')
                )
            readyList @ (narrow stripped)
    
    narrow candidates

let part2 (lines: list<string>) : string =
    let foods = lines |> List.map parseFood
    let allAllergens = foods |> List.collect (fun f -> f.allergens) |> List.distinct
    let inertIngredients = findInertIngredients foods |> Set.ofList
    
    let foodsWithoutInertIngredients =
        foods
        |> List.map (fun f ->
            let iSet = f.ingredients |> Set.ofList
            let ingredients' = Set.difference iSet inertIngredients |> Set.toList
            { f with ingredients = ingredients' }
            )
    
    let ingredientsPerAllergen =
        getIngredientsPerAllergen foodsWithoutInertIngredients allAllergens
        |> Map.toList
        |> List.map (fun (a, is) -> (a, Set.toList is))
        
    let narrowed = narrowAllergenCandidates ingredientsPerAllergen
    let sorted = narrowed |> List.sortBy fst
    let result = String.concat "," (List.map snd sorted)
    
    result

[<Test>]
let Test1 () =
    let res = part1 testLines
    Assert.That(res, Is.EqualTo(5))
    
[<Test>]
let Part1 () =
    let res = part1 (readInput())
    Assert.That(res, Is.EqualTo(2302))
    
[<Test>]
let Test2 () =
    let res = part2 testLines
    Assert.That(res, Is.EqualTo("mxmxvkd,sqjhc,fvjkl"))
    
[<Test>]
let Part2 () =
    let res = part2 (readInput())
    Assert.That(res, Is.EqualTo("smfz,vhkj,qzlmr,tvdvzd,lcb,lrqqqsg,dfzqlk,shp"))