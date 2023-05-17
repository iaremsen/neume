module Neume.Program

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open MathNet.Numerics
open Neume.Signal
open Neume.Music.Pitch
open Neume.Music.Rhythm
open Neume.Music.Tonality.CommonPractice
open Neume.Music.Tonality.Specialized
open Neume.TUI

(***
       The entry point for the software.
***)

let fundamental : Freq = 440.<hertz>;

let min2 = 16M / 15M
let maj3 = 5M / 4M
let per4 = 4M / 3M
let dim6 = 40M / 27M
let neu7 = 11M / 6M

let tun : Tuning = [min2; maj3; per4; dim6; neu7; octave]

let names = ["Ionian";"Dorian";"Phrygian";"Lydian";"Mixolydian";"Aeolian";"Locrian"]

let DFlat : PitchClass = ("D", Flat)
let CFlat : PitchClass = ("C", Flat)
let FSharp : PitchClass = ("F", Sharp)
let BNat : PitchClass = ("B", Natural)

printfn "D♭ Ionian: %A" (KeyMapping DFlat ionian anglo)
printfn "C♭ Aeolian: %A" (KeyMapping CFlat (ionian >>> 5) anglo)
printfn "F♯ Lydian: %A" (KeyMapping FSharp (ionian >>> 3) anglo)
printfn "B Dorian: %A" (KeyMapping BNat (ionian >>> 1) anglo)
