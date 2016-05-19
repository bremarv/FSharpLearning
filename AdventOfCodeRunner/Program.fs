// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open AdventOfCode
open System.Text.RegularExpressions

type Part = { part:int; description:string; solution: unit -> string}
type Day = { description:string; day: int; parts: Part list}

let days = [
        { day=1; description="Not Quite Lisp"; parts =[
            {part=1; description="What floor does Santa end up on"; solution=Day1.solution1 >> string}
            {part=2; description="Steps until floor -1"; solution=Day1.solution2 >> string}
        ]};
        { day=2; description="I Was Told There Would Be No Math"; parts =[
            {part=1; description="Box wrapping"; solution=Day2.solution1 >> string}
            {part=2; description="Ribbons"; solution=Day2.solution2 >> string}
        ]};
        { day=3; description="Perfectly Spherical Houses in a Vacuum"; parts =[
            {part=1; description="Houses visited by santa"; solution=Day3.solution1 >> string}
            {part=2; description="Houses visited by santa and robot"; solution=Day3.solution2 >> string}
        ]};
        { day=4; description="The Ideal Stocking Stuffer"; parts =[
            {part=1; description="MD5 starting with 5 zeroes"; solution=Day4.solution1 >> string}
            {part=2; description="MD5 starting with 6 zeroes"; solution=Day4.solution2 >> string}
        ]};
        { day=5; description="Doesn't He Have Intern-Elves For This?"; parts =[
            {part=1; description="Nice strings part 1"; solution=Day5.solution1 >> string}
            {part=2; description="Nice strings part 2"; solution=Day5.solution2 >> string}
        ]};
        { day=6; description="Probably a Fire Hazard"; parts =[
            {part=1; description="Total lights on"; solution=Day6.solution1 >> string}
            {part=2; description="Brightness"; solution=Day6.solution2 >> string}
        ]};
        ]};
    ]

let (|AdventOfCodeInput|_|) input =
    if input = null then None
    else
        let m = Regex.Match(input, "^([0-9]+) ([0-9]+)$", RegexOptions.Compiled)
        if m.Success then 
            let x = [for x in m.Groups -> x.Value]
                    |> List.tail
                    |> List.map System.Int32.Parse
            Some x
        else None

type SolutionSearchResult = InvalidInput | NoDay | NoPart | Solution of (unit -> string)    

let getSolution input =
    match input with
        | AdventOfCodeInput [x;y] ->
            let day = days |> List.tryFind (fun d -> d.day = x)
            match day with
                | None -> NoDay
                | Some x -> 
                    let part = x.parts |> List.tryFind (fun p -> p.part = y)
                    if part.IsSome then Solution part.Value.solution else NoPart
        | _ -> InvalidInput

let rec readLines () = seq {
    let line = System.Console.ReadLine()
    if line <> "" then
        yield line
        yield! readLines()
}

let printChallenges () =
    let rec printParts parts =
        match parts with
        | [] -> ()
        | x::xs -> 
            printfn "\t%d) %s" x.part x.description
            printParts xs
    let rec printDays days =
        match days with
        | [] -> ()
        | x::xs -> 
            printfn "%d) %s" x.day x.description
            printParts x.parts
            printDays xs
    printDays days
let printSolution s =
    let output =
        match s with
        | InvalidInput -> "Invalid input"
        | NoDay -> "Invalid day"
        | NoPart -> "Invalid part"
        | Solution f -> f ()
    printfn "%s" output
let processInput input =
    match input with
    | "l" -> printChallenges ()
    | x -> getSolution x |> printSolution

[<EntryPoint>]
let main argv =
    printChallenges ()
    readLines ()
    |> Seq.iter processInput    
    0 // return an integer exit code
