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
    
let part1 (lines: list<string>) : int =
    let foods = lines |> List.map parseFood
    let allAllergens = foods |> List.collect (fun f -> f.allergens) |> List.distinct
    let allIngredients = foods |> List.collect (fun f -> f.ingredients) |> List.distinct

    let ingrPerAllergen =
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
    let candidateIngr = Set.unionMany (Map.toList ingrPerAllergen |> List.map snd)
        
    let ingredientsNotListed =
        allIngredients
        |> List.where (fun i -> not (Set.contains i candidateIngr))
        
    let foodAppearance =
        ingredientsNotListed
        |> List.map (fun i ->
            foods |> List.where (fun f -> List.contains i f.ingredients) |> List.length
            )
        |> List.sum
        
    foodAppearance

[<Test>]
let Test1 () =
    let res = part1 testLines
    Assert.That(res, Is.EqualTo(5))
    

[<Test>]
let Part1 () =
    let res = part1 (readInput())
    Assert.That(res, Is.EqualTo(2302))