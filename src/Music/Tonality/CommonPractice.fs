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

type Quality = Major | Minor
type Scale = int Set // uint?
type Key = PitchClass * Quality

let chromatic : Scale = Set.ofList [1..12] // root omitted; last element is period
let ionian : Scale = Set.ofList [2;4;5;7;9;11;12]


let Period (s : Scale) = Set.maxElement s

let ReSeat n s p =
    match (n - s) > 0 with
        | true -> n - s
        | false -> (n - s) + p

let (>>>) (s : Scale) (n : int) =
    let p = Period s
    let sh =  n % (Set.count s)
    let i = (Set.toList s)[sh]
    Set.ofList [ for it in s do yield (ReSeat it i p) ]


let BinEnc (s : Scale) =
    [ for n in s do yield (2. ** float (n - 1)) ] |> List.sum |> int

//let ForteNo (s : Scale) =
//let PrimeMode (s : Scale) =

//type Chord =

// let isEnharmonic =
// TO-DO
