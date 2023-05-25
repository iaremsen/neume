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
    let (oRow, oCol, mRows, mCols) = (Console.CursorTop, Console.CursorLeft,
                                      Console.WindowHeight - 1, Console.WindowWidth - 1)

    for i in 1..(mCols) do
        writeAt '═' (i, 0)
        writeAt '═' (i, mRows)

    for i in 1..(mRows) do
        writeAt '║' (0, i)
        writeAt '║' (mCols, i)

    writeAt '╔' (0, 0)
    writeAt '╚' (0, mRows)
    writeAt '╗' (mCols, 0)
    writeAt '╝' (mCols, mRows)
