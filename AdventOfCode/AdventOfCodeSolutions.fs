﻿namespace AdventOfCode

module InputHelp =    
    let explode (s:string) = 
        [for c in s -> c]

    let GetCharacterInput s =
        System.IO.File.ReadAllLines s
        |> Array.head
        |> explode

    let GetLineInput =
        System.IO.File.ReadLines

module ActualCode =
    open System.Text.RegularExpressions

    let min x y = 
        if x < y then x else y
    let (|StartsWith|_|) needle (haystack:string) = 
        if haystack.StartsWith(needle) then Some () else None

    let countFloors =
        let moveFloor current input =            
            current + match input with 
                        | '(' -> 1
                        | ')' -> -1
                        | _ -> 0

        let rec realCountFloors step floor characters =
            match characters with
            | [] -> [(step, floor)]
            | x::xs -> (step, floor)::realCountFloors (step + 1) (moveFloor floor x) xs

        realCountFloors 0 0
        
    let (|MatchBoxSize|_|) input =
        if input = null then None
        else
            let m = Regex.Match(input, "^([0-9]+)x([0-9]+)x([0-9]+)$", RegexOptions.Compiled)
            if m.Success then 
                let x = [for x in m.Groups -> x.Value]
                        |> List.tail
                        |> List.map System.Int32.Parse
                Some x
            else None

    let parseBox s =
        match s with
            | MatchBoxSize [l;w;h] -> Some (l, w, h)
            | _ -> None

    let boxWrapping (l, w, h) =
        let sides = [l*w; w*h; h*l]
        let surfaceArea = List.sum sides * 2

        let boxSlack = List.reduce (fun acc x -> min acc x) sides

        surfaceArea + boxSlack

    let ribbonLength (l, w, h) =
        let bow = l*w*h
        let wrap = 
            [l;w;h]
            |> List.sort
            |> List.take 2
            |> (List.sum >> (*) 2)
        wrap + bow
        
    let nextLocation (north, east) move =
        match move with
        |'<' -> (north, east-1)
        |'>' -> (north, east+1)
        |'^' -> (north+1, east)
        |'v' -> (north-1, east)
        |_ -> (north, east)

    let rec moveSanta pos moves =
        match moves with
            | [] -> [pos]
            | x::xs -> pos::moveSanta (nextLocation pos x) xs

    let generateMd5 key = 
        let md5 = System.Security.Cryptography.MD5.Create()
        let doHash key num =
            key + (string num)
            |> System.Text.Encoding.ASCII.GetBytes
            |> md5.ComputeHash
            |> Array.map (fun x -> System.String.Format("{0:X2}", x))
            |> System.String.Concat
        let rec innerGenerate key num = seq {
            yield (doHash key num, num)
            yield! innerGenerate key (num+1)
        }
        innerGenerate key 0

    type Day5String = Nice | Naughty        
    let niceRegexesPart1 = ["^(?!.*(ab|cd|pq|xy)).*$";"([a-z])\1";"([aeiou].*){3}"]
    let niceRegexesPart2 = ["([a-z]{2}).*\1";"([a-z])[a-z]\1"]
    let appraiseString2 regexes s =
        let res = regexes |> List.tryFind (fun r -> Regex.IsMatch(s, r) |> not)
        if res = None then Nice else Naughty

open ActualCode
open InputHelp
module Day1 =
    module private hidden =
        let file = "InputDay1.txt"

    open hidden
    let solution1 () =
        countFloors (GetCharacterInput file)
        |> List.last
        |> snd

    let solution2 () =
        let steps = 
            countFloors (GetCharacterInput file)
            |> List.find (fun (_, floor) -> floor = -1)
            |> fst
        steps

module Day2 =
    module private hidden =
        let file = "InputDay2.txt"
        let boxValue f box =
            match box with
            | Some x -> f x
            | None -> 0
        
    open hidden
    let solution1 () =
        GetLineInput file
        |> Seq.map (parseBox >> (boxValue boxWrapping))
        |> Seq.sum

    let solution2 () =
        GetLineInput file
        |> Seq.map (parseBox >> (boxValue ribbonLength))
        |> Seq.sum

module Day3 =
    module private hidden =
        let file = "InputDay3.txt"
        
    open hidden
    let solution1 () =
        moveSanta (0, 0) (GetCharacterInput file)
        |> List.distinct
        |> List.length
    let solution2 () =
        let splitList = List.foldBack (fun x (l, r) -> x::r, l) (GetCharacterInput file) ([],[])
        let santaLocations = moveSanta (0, 0) (fst splitList)
        let robotLocations = moveSanta (0, 0) (snd splitList)
        List.append santaLocations robotLocations
        |> List.distinct
        |> List.length

module Day4 =
    module private hidden =
        let findStartingWith x =
            generateMd5 "iwrupvqb"
            |> Seq.find (fun (hash,_) -> hash.StartsWith(x))
            |> snd

    open hidden
    let solution1 () = findStartingWith "00000"
    let solution2 () = findStartingWith "000000"
   
module Day5 =
    module hidden=
        let file = "InputDay5.txt"
        let countStrings niceRegexes =
            GetLineInput file
            |> Seq.map (appraiseString2 niceRegexes)
            |> Seq.filter (fun x -> x = Nice)
            |> Seq.length

    open hidden
    let solution1 () = countStrings niceRegexesPart1
    let solution2 () = countStrings niceRegexesPart2

module DayX =
    module hidden=
        let file = "InputDay5.txt"

    open hidden
    let solution1 () = "Not implemented"
    let solution2 () = "Not implemented" 
