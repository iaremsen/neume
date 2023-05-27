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

let chromatic : Scale = Set.ofList [1..12] // root omitted; last element is the period/modulus (e.g. octave conventionally) of the scale
let ionian : Scale = Set.ofList [2;4;5;7;9;11;12]
let pent : Scale = Set.ofList [2;4;7;9;12]

let Period (s : Scale) : int = s.MaximumElement

let inline charToInt c = int c - int '0'

let BinEnc (s : Scale) : int = // a unique, *dense* binary encoding for every scale of any cardinality, as an integer
    [ for n in s -> 2. ** float (n - 1) ] |> List.sum |> int

let rec BinDcd (x : int) : Scale =
    Convert.ToString(x, 2).ToCharArray() |> Array.rev |> Array.indexed |> Array.filter(fun (x,y) -> y <> '0') |> Array.map(fun (x,y) -> x * (y |> charToInt) + 1) |> Array.toList |> Scale

let (%.) (s : Scale) (n : int) = (Set.toList s)[n % s.Count]
let (%>) (s : Scale) (n : int) = s %. (n - 1)



let ReSeat x offset modulus : int = // determines the relationship of `offset` to `x` (mod `modulus`)
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

type Letterform = string // best way to do this?
type Accidental = int * string
type Alphabet = { Letterforms : Letterform List;
                  Accidentals : Map<int, string>;
                  Spacing : Scale }
type PitchClass = Letterform * Accidental
type Degree = int * Accidental // ordinal

let (>@>) (lf : List<'a>) (offset : int) : List<'a> =
    let index = (offset % lf.Length)
    match index with
        | 0 -> lf
        | _ -> List.skip index lf @ List.take index lf

let (<@<) lf (offset : int) = lf >@> lf.Length - offset

let reDup str n = ("", List.replicate n str) ||> List.fold (fun s v -> s + v)

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

// TO-DO: make sure there’s a fallback for spamming accidentals, 3♯/♭ &c.
let KeyMapping (tonic : PitchClass) (s : Scale) (a : Alphabet) =
    // cycle through both `a`’s Letterform List and `a`’s Scale until the first
    // element of the former matches `tonic.fst`
    // if `tonic` has an accidental, set it to all notes of the scale
    //
    // compare `s` to `a`’s scale, use difference to decide further accidentals
    ////// e.g. D♭ major ⇥ [D♭;E♭;F♮;G♭;A♭;B♭;C♮] ∵
    ////// D♭_Dorian = A_Minor >>> 4 ||> ♭ = [D♭;E♭;F♭;G♭;A♭;B♭;C♭]⸻
    ////// dorian[2] = 3 < major[2] = 4⸻
    let s_l = Set.toList s
    let pivot = List.findIndex (fun x -> x = fst tonic) a.Letterforms // pos of tonic in alphabet used
    let delta = tonic |> snd |> fst // accidental of root note
    let notes = a.Letterforms >@> (pivot + 1)
    [ for i in 0..(s_l.Length - 1) -> (notes[i], a.Accidentals[delta + s_l[i] - (Set.toList (a.Spacing >>> pivot))[i]] ) ] <@< 1

//let ContainsSubset (x : Scale) (y : Scale) =
    // etc
let RootTriad (s : Scale) =
    Scale (Set.empty.Add(0).Add(s %> 2).Add(s %> 4))

let Triads (s : Scale) =
    [for m in (Modes s) -> RootTriad m]
