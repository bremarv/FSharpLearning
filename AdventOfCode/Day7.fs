namespace AdventOfCode

open Utility
module Day7 =
    module Hidden =
        type Signal =
            | Id of string
            | Source of int16
        type Gate =
            | Not of Signal
            | And of Signal * Signal
            | Or of Signal * Signal
            | LShift of Signal * Signal
            | RShift of Signal * Signal
        type Component =
            | Signal of Signal
            | Gate of Gate
            | Wire of Component
        

    open Hidden
    let solution1 () = ""
    let solution2 () = ""