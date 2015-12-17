let formatResult (x:Option<int>) =
    match x with
    | None -> "Failure"
    | Some i -> System.String.Format("Parsed {0}", i)

let tryParseOption intStr = 
   try
      let i = System.Int32.Parse intStr
      Some i
   with _ -> None

//let parseAndPrint = tryParseOption >> formatResult >> printfn "%s"

let rec readLines () = seq {
    let line = System.Console.ReadLine()
    if line <> "" then
        yield line
        yield! readLines()
}

let parseAndPrintInput () =
    printfn "%s" "starting"
    readLines()
    |> Seq.map (tryParseOption >> formatResult)
    |> Seq.iter (printfn "%s")

let explode (s:string) =
    [for c in s -> c]

[<EntryPoint>]
let main argv =
    let res = AdventOfCode.Solution.day3.solution2
    printfn "%d" res
    0 // return an integer exit code
