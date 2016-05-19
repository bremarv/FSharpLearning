namespace AdventOfCode
module Utility =
    let explode (s:string) = 
        [for c in s -> c]

    let GetCharacterInput s =
        System.IO.File.ReadAllLines s
        |> Array.head
        |> explode

    let GetLineInput =
        System.IO.File.ReadLines        

    let min x y = 
        if x < y then x else y
    let max x y =
        if x < y then y else x

    let (|StartsWith|_|) needle (haystack:string) = 
        if haystack.StartsWith(needle) then Some () else None