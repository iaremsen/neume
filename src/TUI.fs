module Neume.TUI

open System

(***
       Drawing and managing the text user interface.
***)
// TO-DO: color codes &c.

let writeAt (c : Char) ((x, y) : int * int) =
    try
        Console.SetCursorPosition(x, y)
        Console.Write(c)
    with :? ArgumentOutOfRangeException as e ->
        Console.Clear()
        printfn $"{e.Message}"

let drawBorder =
    let (max_y, max_x) = (Console.WindowHeight - 1, Console.WindowWidth - 1)

    writeAt '╔' (0, 0)
    writeAt '╚' (0, max_y)
    writeAt '╗' (max_x, 0)
    writeAt '╝' (max_x, max_y)
    for i in 1..(max_x) do
        writeAt '═' (i, 0)
        writeAt '═' (i, max_y)
    for i in 1..(max_y) do
        writeAt '║' (0, i)
        writeAt '║' (max_x, i)
