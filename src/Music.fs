
module Neume.Music

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open MathNet.Numerics
open Neume.Signal

/// pitch (non-perceptual frequency) ///
type Ratio = BigRational
type Scale = Ratio list // TO-DO: support non-ratio-based scales
[<Measure>] type cent

let octave = Ratio.FromIntFraction(2, 1)
let tritave = Ratio.FromIntFraction(3, 1)

let ToCents (r : Ratio) = 1200.<cent> * Math.Log2(float r) // TO-DO: non-log2-based cycles
let ShiftCents (f : Freq) (c : float<cent>) = (2.**(float c/1200.) * f)
let rec RatioAboveRoot (p : Ratio) (period : Ratio) =
    match (p >= Ratio.One) with
        | true -> p
        | false -> (RatioAboveRoot (p * period) period)
// TO-DO: P-limit from cents -> Ratio
// TO-DO: let PadScale (s : Scale) (period : Ratio) =
let IntervalTable(s: Scale) =
    let period = List.last s
    [ for p in s do yield s |> List.map(fun x -> x / p) |> List.map(fun x -> RatioAboveRoot x period) |> List.sort |> List.tail ]

/// rhythm (perceptual frequency) ///
type TimeSignature = int * int // abstract measure of time value; might present as a non-reduced decimal multiple i.e. 15/16 → 3.75/4
type Tempo = float * int // beats per second * value of said beat
