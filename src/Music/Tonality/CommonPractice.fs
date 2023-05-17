module Neume.Music.Tonality.CommonPractice

open System

(***
       Music-theoretical constructs well-known and developed during the Western
       ‘common practice' period. In other words, normal stuff goes here: what
       one learns in an Introduction to Music Theory class, plus descendant
       ideas and techniques used in popular recorded music.
***)

(***
        SCALES
***)

// TO-DO integrate and make rigid beyond chromatic scale/12EDO
type Scale = int Set // uint?

let chromatic : Scale = Set.ofList [1..12] // root omitted; last element is the period/modulus of the scale
let ionian : Scale = Set.ofList [2;4;5;7;9;11;12]
let pent : Scale = Set.ofList [2;4;7;9;12]

let Period (s : Scale) : int = s.MaximumElement

let BinEnc (s : Scale) : int = // a unique, *dense* binary encoding for every scale of any cardinality, as an integer
    [ for n in s -> 2. ** float (n - 1) ] |> List.sum |> int

let ReSeat x offset modulus : int = // determines the relationship of `offset` to `x` mod `modulus`
    let diff = x - offset
    match diff > 0 with
        | true -> diff
        | false -> diff + modulus

let (>>>) (s : Scale) (offset : int) : Scale = // modulates a scale; returns the nth mode of a scale; inverts chords
    let index = (offset % s.Count)

    match index with
        | 0 -> s
        | _ -> Set.map(fun x -> ReSeat x (Set.toList s).[index - 1] (Period s)) s

let (<<<) (s : Scale) (offset : int) = s >>> s.Count - offset

let Modes(s : Scale) : List<Scale> = // returns all the modes of a scale; all the inversions of a chord.
    [ for i in 0..(s.Count-1) -> s >>> i ]

let PrimeMode (s : Scale) : Scale = // smallest binary-encoded/most "left-compact" mode; root inversion of a chord
    let x = [ for i in Modes s -> (BinEnc i, i) ]
    (List.sort x)[0] |> snd

//let ForteNo (s : Scale) =

// let isEnharmonic =
// TO-DO

(***
        CHORDS
***)

type Chord = Scale // it’s that easy
let major : Chord = Set.ofList [4;7;12]
let minor : Chord = Set.ofList [3;7;12]

type Degree = int // ordinal,
//type RomanNumeral =

type Letterform = string // best way to do this?
type Accidental = int * string
type Alphabet = { Letterforms : Letterform List;
                  Accidentals : Map<int, string>;
                  Spacing : Scale }
type PitchClass = Letterform * Accidental

let (>@>) (lf : Letterform List) (offset : int) : Letterform List =
    let index = (offset % lf.Length)
    match index with
        | 0 -> lf
        | _ -> List.skip index lf @ List.take index lf

let (<@<) (lf : List) (offset : int) : List = lf >@> lf.Length - offset

let DblFlat : Accidental = (-2, "𝄫")
let Flat : Accidental = (-1, "♭")
let Natural : Accidental = (0, "♮")
let Sharp : Accidental = (1, "♯")
let DblSharp : Accidental = (2, "𝄪")

let anglo : Alphabet = {Letterforms = [for c in 'A'..'G' -> string c];
                        Accidentals = Map[DblFlat;Flat;Natural;Sharp;DblSharp];
                        Spacing = ionian >>> 5}
let german : Alphabet = {Letterforms = ["A";"B";"H";"C";"D";"E";"F";"G"];
                         Accidentals = Map[DblFlat;Flat;Natural;Sharp;DblSharp];
                         Spacing = (ionian >>> 5).Add(4)}
let solfege : Alphabet = {Letterforms = ["do";"re";"mi";"fa";"sol";"la";"ti"];
                          Accidentals = Map[DblFlat;Flat;Natural;Sharp;DblSharp];
                          Spacing = ionian}
let hindustani : Alphabet = {Letterforms = ["सा";"रे";"ग";"म";"प";"ध";"नि"];
                             Accidentals = Map[];
                             Spacing = ionian}

type Quality = Major | Minor

type Key = PitchClass * Quality

let KeyMapping (tonic : PitchClass) (s : Scale) (a : Alphabet) =
    // cycle both `a`’s Letterform List and `a`’s Scale until the first element of the former matches `tonic.fst`
    // if `tonic` has an accidental, set it (sharp/flat)
    // compare `s` to `a`’s scale, use difference to decide further accidentals (i.e. in D♭ major, D♭ ⇢ E♭ ⇢ F♮ because (anglo.scale >>> 4)[2] = 3 < major[2] = 4, and Natural is 1 greater than Flat)
    //maybe
    let i = List.findIndex (fun x -> x = fst tonic) a.Letterforms
    let ls = (Set.toList s, Set.toList (a.Spacing >>> i))
    let diff = [ for i in 0..(s.Count - 1) -> (fst ls)[i] - (snd ls)[i] ]
    let accs = [ for i in diff -> a.Accidentals[((tonic |> snd |> fst) + i)]]
    let notes = a.Letterforms >@> (i + 1)
    [ for it in 0..(s.Count - 1) -> (notes[it], a.Accidentals[((tonic |> snd |> fst) + diff[it])]) ] <@< 1
