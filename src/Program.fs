module Neume.Program

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open Neume.Signal
open Neume.Music.Pitch
open Neume.Music.Rhythm
open Neume.Music.Tonality.CommonPractice
open Neume.Music.Tonality.Specialized
open Neume.TUI

(***
       The entry point for the software.
***)

module Kernel =
    [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
    extern bool SetConsoleOutputCP(uint32 wCodePageID)
if Environment.OSVersion.Platform = PlatformID.Win32NT then Kernel.SetConsoleOutputCP 65001u |> ignore

let fundamental : Freq = 440.<hertz>;

let DFlat : PitchClass = {name = "D"; acc = Flat}
let CFlat : PitchClass = {name = "C"; acc = Flat}
let FSharp : PitchClass = {name = "F"; acc = Sharp}
let BNat : PitchClass = {name = "B"; acc = Natural}

let BMaj : Tonality = (anglo, BNat, ionian)

let MinorThird : Interval = {step = 3; size = 3}
// printfn "D♭ Diatonic: %A" (KeyMapping DFlat diatonic anglo)
// printfn "C♭ Aeolian: %A" (KeyMapping CFlat (diatonic >>> 5) anglo)
// printfn "F♯ Lydian: %A" (KeyMapping FSharp (diatonic >>> 3) anglo)
// printfn "B Dorian: %A" (KeyMapping BNat (diatonic >>> 1) anglo)

Spelling {name = "C"; acc = Natural} {step = 4; size = 6} anglo
Spelling {name = "C"; acc = Natural} {step = 5; size = 6} anglo
Spelling {name = "C"; acc = Natural} {step = 2; size = 3} anglo
Spelling {name = "C"; acc = Natural} {step = 10; size = 8} anglo

//////////////////////////////////////////////////////////////////////
// let mutable cki = Unchecked.defaultof<ConsoleKeyInfo>            //
// let mutable (x, y) = (Console.WindowHeight, Console.WindowWidth) //
//                                                                  //
// Console.Clear()                                                  //
// drawBorder                                                       //
// while cki.Key <> ConsoleKey.Escape do                            //
//     while not Console.KeyAvailable do                            //
//         Threading.Thread.Sleep 250                               //
//     cki <- Console.ReadKey true                                  //
// Console.Clear()                                                  //
//////////////////////////////////////////////////////////////////////
