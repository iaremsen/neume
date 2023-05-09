// For more information see https://aka.ms/fsharp-console-apps
module Neume.Signal

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

let tau = 2.0 * Math.PI

type sampleRate = float<hertz>
type Freq = float<hertz>

type AudioSignal = float seq * sampleRate
(* type Signal =
    | CSignal of fx : Domain
    | AudioSignal of Pressure seq * sampleRate
    | CEnvelope of CSignal

type CSignal = * *)
let PulseWave (f : Freq) (length : float<second>) (phase : float) =
    let inflection = (f * phase) - (f / 2) >> Math.Abs
