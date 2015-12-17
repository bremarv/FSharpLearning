namespace AdventOfCode

module Solution =
    module ActualCode =
        open System.Text.RegularExpressions

        let explode (s:string) = 
            [for c in s -> c]
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

    open ActualCode

    module day1 =
        let rawInput = Array.head (System.IO.File.ReadAllLines("InputDay1.txt"))
        let input = explode rawInput
        let solution1 =
            countFloors input
            |> List.last
            |> snd

        let solution2 =
            let steps = 
                countFloors input
                |> List.find (fun (_, floor) -> floor = -1)
                |> fst
            steps

    module day2 =
        let input = System.IO.File.ReadLines("InputDay2.txt")
        let boxValue f box =
            match box with
            | Some x -> f x
            | None -> 0
        
        let solution1 =
            input
            |> Seq.map (parseBox >> (boxValue boxWrapping))
            |> Seq.sum

        let solution2 =
            input
            |> Seq.map (parseBox >> (boxValue ribbonLength))
            |> Seq.sum

    module day3 =
        let rawInput = Array.head (System.IO.File.ReadAllLines("InputDay3.txt"))
        let input = explode rawInput
        let solution1 =
            moveSanta (0, 0) input
            |> List.distinct
            |> List.length
        let solution2 =
            let splitList = List.foldBack (fun x (l, r) -> x::r, l) input ([],[])
            let santaLocations = moveSanta (0, 0) (fst splitList)
            let robotLocations = moveSanta (0, 0) (snd splitList)
            List.append santaLocations robotLocations
            |> List.distinct
            |> List.length

    module day4 =
        let findStartingWith x =
            generateMd5 "iwrupvqb"
            |> Seq.find (fun (hash,_) -> hash.StartsWith(x))
            |> snd
        let solution1 = findStartingWith "00000"
        let solution2 = findStartingWith "000000"