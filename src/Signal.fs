﻿module Neume.Signal

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

(***
       Types and functions for manipulating signals and converting between
       abstract mathematical objects and digital data.
***)
let (|Neg|NonNeg|) input = if input >= 0 then NonNeg else Neg

let tau = 2.0 * Math.PI

type SampleRate = decimal<hertz>
type Freq = float<hertz>

type AudioSignal = float seq * SampleRate

// let PulseWave (f : Freq) (length : float<second>) (phase : float) =
//     let inflection = (f * phase) - (f / 2) >> Math.Abs
