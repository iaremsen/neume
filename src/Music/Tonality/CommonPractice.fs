module Neume.Music.Tonality.CommonPractice

open System

(***
       Music-theoretical constructs well-known and developed during the Western
       ‘common practice' period. In other words, normal stuff goes here: what
       one learns in an Introduction to Music Theory class, plus descendant
       ideas and techniques used in popular recorded music.
***)
// TO-DO integrate and make rigid beyond chromatic scale/12EDO
type PCLetter = A | B | C | D | E | F | G // best way to do this?
type Accidental = Flat | Natural | Sharp
type PitchClass = PCLetter * Accidental
// type RomanNumeral =


type Quality = Major | Minor
type Scale = int Set // uint?
type Key = PitchClass * Quality

let chromatic : Scale = Set.ofList [1..12] // root omitted; last element is the period/modulus of the scale
let ionian : Scale = Set.ofList [2;4;5;7;9;11;12]

let Period (s : Scale) = s.MaximumElement

let ReSeat x offset modulus =
    let diff = x - offset
    match diff > 0 with
        | true -> diff
        | false -> diff + modulus

let (>>>) (s : Scale) (offset : int) =
    let index = (offset % s.Count)

    if index = 0 then s
    else
        let newRoot = (Set.toList s)[index - 1]
        Set.map(fun x -> ReSeat x newRoot (Period s)) s

let (<<<) (s : Scale) (offset : int) = s >>> s.Count - offset

let BinEnc (s : Scale) =
    [ for n in s -> 2. ** float (n - 1) ] |> List.sum |> int

//let ForteNo (s : Scale) =

let Modes(s : Scale) =
    [ for i in 0..(s.Count-1) -> s >>> i ]

let PrimeMode (s : Scale) =
    let m = Modes s
    let x = [ for i in m -> (BinEnc i, i) ]
    (List.sort x)[0] |> snd

//type Chord =

// let isEnharmonic =
// TO-DO
