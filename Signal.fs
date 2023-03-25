// For more information see https://aka.ms/fsharp-console-apps
module Signals

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

let tau = 2.0 * Math.PI

type Complex(r : double, i : double) =
    member this.r = r
    member this.i = i

    static member (+) (x, z : Complex) =
        Complex(z.r + x, z.i)

    static member (*) (x, z : Complex) =
        Complex(z.r * x, z.i * x)

    static member (*) (x : Complex, z : Complex) =
        Complex(z.r * x.r - z.i * x.i, z.r * x.r + z.i * x.i)
//    static member (/) (x, z : Complex) =


let conj(z : Complex) = Complex(z.r, z.i * -1.)

type CSignal = double

type sampleRate = float<hertz>
type Freq = float<hertz>

type AudioSignal = float seq * sampleRate

(* type Signal =
    | CSignal of fx : Domain
    | AudioSignal of Pressure seq * sampleRate
    | CEnvelope of CSignal

type CSignal = *)
