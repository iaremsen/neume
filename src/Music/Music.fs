namespace Neume.Music

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open Neume.Signal

(***
       What’s music-specific and what’s not? Please don’t ask me.
***)

(*** Pitch [a.k.a. paraperceptual frequency] ***)
module Pitch =
    type Interval = Decimal
    type Tuning = Interval list
    [<Measure>] type cent

    let octave = 2M
    let tritave = 3M

// TO-DO: non-log2-based cycles (bohlen-pierce, &c.)
    let ToCents (i : Interval) = 1200.<cent> * Math.Log2(float i)

    let ShiftCents (f : Freq) (c : float<cent>) = (2.**(float c/1200.) * f)
    let rec RatioAboveRoot (i : Interval) (period : Interval) =
        match (i >= 1M) with
            | true -> i
            | false -> RatioAboveRoot (i * period) (period)
// TO-DO: P-limit from cents -> Ratio
// TO-DO: let PadScale (s : Scale) (period : Ratio) =
    let IntervalTable(tun : Tuning) =
        let period = List.last tun
        [ for q in tun do yield tun |> List.map(fun x -> x / q) |> List.map(fun x -> RatioAboveRoot x period) |> List.sort |> List.tail ]

(*** Rhythm [a.k.a. perceptual frequency] ***)
module Rhythm =
    type TimeSignature = int * int // abstract measure of time value; might present as a non-reduced decimal multiple i.e. 15/16 → 3.75/4
    type Tempo = float * int // beats per second * value of said beat
