module Neume.TUI

open System

(***
       Drawing and managing the text user interface.
***)
let origRow = Console.CursorTop
let origCol = Console.CursorLeft

let writeAt s x y =
    try
        Console.SetCursorPosition(origCol + x, origRow + y)
        printfn $"%s{s}"
    with :? ArgumentOutOfRangeException as e ->
        Console.Clear()
        printfn $"{e.Message}"
