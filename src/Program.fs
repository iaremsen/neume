module Neume.Program

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open MathNet.Numerics
open Neume.Signal
open Neume.Music
open Neume.TUI

let fundamental : Freq = 440.<hertz>;

let min2 = Ratio.FromIntFraction(16,15)
let maj3 = Ratio.FromIntFraction(5,4)
let per4 = Ratio.FromIntFraction(4,3)
let dim6 = Ratio.FromIntFraction(40,27)
let neu7 = Ratio.FromIntFraction(11,6)
let octave = Ratio.FromIntFraction(2,1)

let t = Ratio.FromIntFraction(2,11)


let s : Scale = [min2; maj3; per4; dim6; neu7; octave]

printfn $"%A{IntervalTable s}"
