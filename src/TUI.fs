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

    Console.ForegroundColor <- ConsoleColor.Cyan
    writeAt '╔' (0, 0)
    writeAt '╚' (0, max_y)
    writeAt '╗' (max_x, 0)
    writeAt '╝' (max_x, max_y)
    for i in 1..(max_x-1) do
        writeAt '═' (i, 0)
        writeAt '═' (i, max_y)
    for i in 1..(max_y-1) do
        writeAt '║' (0, i)
        writeAt '║' (max_x, i)
    Console.BackgroundColor <- ConsoleColor.Blue
    Console.ForegroundColor <- ConsoleColor.Black
    Console.SetCursorPosition(1, max_y)
    Console.Write("NEUME")
    Console.ResetColor()
    Console.SetCursorPosition(1,1)
