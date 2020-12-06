// Learn more about F# at http://fsharp.org

open System
open System.Linq
open System.Text.RegularExpressions
open fsutils.FsUtils

let requiredFields = [ "byr" ; "iyr" ; "eyr" ; "hgt" ; "hcl" ; "ecl" ; "pid" ; "cid" ]

type Passport = { data: Map<string, string> }

let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])
    
let arrayToTuple (arr: 'a[]) : 'a*'a = (arr.[0], arr.[1])

let toPassport (lines: seq<string>) : Passport =
    let build (d: Map<string, string>) (l: string) =
        let tups: seq<string*string> = l.Split(' ').Select(fun part -> arrayToTuple (part.Split(':')))
        join d (Map.ofSeq tups)
    let passportData : Map<string, string> = Seq.fold build Map.empty lines
    { data = passportData }

let rec readPassports (lines: seq<string>) : seq<Passport> =
    readBatches lines |> Seq.map toPassport

let eqList (l1: list<string>) (l2: list<string>) =
    0 = List.compareWith (fun (x: string) (y: string) -> x.CompareTo(y)) l1 l2

let isValid1 (p: Passport) : bool =
    let availableFields : list<string> = List.ofSeq (Map.toSeq p.data |> Seq.map fst)
    let missingFields = List.ofSeq (requiredFields.Except(availableFields))
    (List.length missingFields) = 0 || (eqList missingFields (List.singleton "cid"))
    
let validInt (s: string) (dflt: int) =
    let success, value = Int32.TryParse s
    if success then value else dflt
    
let inRange (v: int) (min: int) (max: int) : bool = min <= v && v <= max
    
let hclRegex = Regex("^#[0-9a-f]{6}$")
let pidRegex = Regex("^[0-9]{9}$")
let eyeColors = [ "amb" ; "blu" ; "brn" ; "gry" ; "grn" ; "hzl" ; "oth" ]
let hgtRegex = Regex("^(?<val>[0-9]+)(?<unit>cm|in)$")

let isFieldValid (tup: string*string) =
    let fieldName = fst tup
    let fieldValue = snd tup
    match fieldName with
    | "byr" -> inRange (validInt fieldValue -1) 1920 2002
    | "iyr" -> inRange (validInt fieldValue -1) 2010 2020
    | "eyr" -> inRange (validInt fieldValue -1) 2020 2030
    | "hgt" ->
        let m = hgtRegex.Match(fieldValue)
        if m.Success then
            let value = (validInt m.Groups.["val"].Value -1)
            let unit = m.Groups.["unit"].Value
            if unit = "cm" then (inRange value 150 193) else (inRange value 59 76)
        else
            false
    | "hcl" -> hclRegex.IsMatch(fieldValue)
    | "ecl" -> eyeColors |> List.contains fieldValue
    | "pid" -> pidRegex.IsMatch(fieldValue)
    | _ -> true
    
let hasValidFields (p: Passport) : bool =
    p.data
        |> Map.toSeq
        |> Seq.forall isFieldValid

let isValid2 (p: Passport) : bool = (isValid1 p) && (hasValidFields p)

let part1 (passports: seq<Passport>) =
    let validCount = passports.Count(fun p -> isValid1 p)
    printf "%d\n" validCount
    
let part2 (passports: seq<Passport>) =
    let validCount = passports.Count(fun p -> isValid2 p)
    printf "%d\n" validCount

[<EntryPoint>]
let main argv =
    let lines = readLines "input"
    let passports = readPassports lines
    part1 passports
    part2 passports
    0 // return an integer exit code
