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
        // TO-DO: design when exactly to count from 0 and when to count from 1
        // (e.g. scale[0] is the *unison*, scale[1] is the *second*, could be confusing)
***)

// TO-DO: integrate and make rigid beyond chromatic scale/12EDO
let inline (>@>) (lst : List<'a>) (offset : int) : List<'a> =
    let index = (offset % lst.Length)
    match index with
        | 0 -> lst
        | _ -> List.skip index lst @ List.take index lst

let inline (<@<) lst (offset : int) = lst >@> lst.Length - offset

let ReSeat x offset modulus : int = // determines the relationship of `offset` to `x` (mod `modulus`)
    let diff = x - offset
    match diff > 0 with
        | true -> diff
        | false -> diff + modulus

type Scale(lst : List<int>) =
    member this.set = Set.ofList lst
    member this.lst = Set.toList this.set
    member this.period = this.set.MaximumElement
    member this.card = this.set.Count

// a unique, *dense* binary encoding for every scale of any cardinality, as an integer
    member this.bin = [ for n in this.set -> 2. ** float (n - 1) ] |> List.sum |> int
    member this.bitfield = Convert.ToString(this.bin, 2).ToCharArray() |> Array.map(fun x -> (int x - int '0')) |> Array.rev |> Array.toList

//    let steps = [ for i in 0..(card - 1) -> abs((s %. i) - (s %. (i - 1))) ]

 // modulates a scale; returns the nth mode of a scale (counting from zero, aeolian is `diatonic |> mode 6`); inverts chords
    static member (>>>) (s : Scale, offset : int) : Scale =
        let index = offset % s.card
        match index with
            | 0 -> s
            | _ -> Scale(Set.toList(Set.map(fun x -> ReSeat x s.lst[index - 1] (s.period)) s.set))

    static member (<<<) (s : Scale, offset : int) = s >>> s.card - offset
    static member (%.) (s : Scale, n : int) = (s.lst)[abs(n % s.card)]
    static member (%>) (s : Scale, n : int) = s %. (n - 2) // music counts from 1; make sure this works with Cycle and Fragment

    static member (+) (s : Scale, n : int) = Scale(s.lst @ [n])
    static member (+) (s : Scale, c : Scale) = Scale(s.lst @ c.lst)

    static member steps (s : Scale) = [ for i in 0..(s.card - 1) -> abs((s %. i) - (s %. (i - 1)))]
    member this.mode (n : int) = Scale(this.lst) >>> (n - 1)

    new() = Scale([0])


// not sure what type safety rammifications i want here, it’s really quite philosophical
let (|Cycle|Fragment|) (s : Scale) = if s.set.MinimumElement = 0 then Fragment else Cycle

let chromatic = Scale([1..12]) // root omitted (i.e. Cycle as above); last element is the period/modulus (e.g. the octave, conventionally) of the scale
let diatonic = Scale([2;4;5;7;9;11;12])
let pent = Scale([2;4;7;9;12])
let fragHWH = Scale([0;1;3;4]) // hmm.

//let BinDcd (x : int) : Scale =
  //  x |> List.indexed |> List.filter(fun (x,y) -> y <> 0) |> List.map(fun (x,y) -> (x * y) + 1) |> Scale()


let ionian : Scale = diatonic.mode(1)
let dorian : Scale = diatonic.mode(2)
let phrygian : Scale = diatonic.mode(3)
let lydian : Scale = diatonic.mode(4)
let mixolydian : Scale = diatonic.mode(5)
let aeolian : Scale = diatonic.mode(6)
let locrian : Scale = diatonic.mode(7)

// returns all the modes of a scale; all the inversions of a chord.
let Modes(s : Scale) : List<Scale> =
    [ for i in 1..(s.card) -> s.mode(i) ]

//let PrimeMode (s : Scale) : Scale = // smallest binary-encoded/most "left-compact" mode; root inversion of a chord

// let ForteNo (s : Scale) =
//
// let isEnharmonic =

(***
        CHORDS
***)

// i think it’s usually best to work with chords as full scales/treat them cyclically, because they are invertible and octave-displacable/create a harmonic space as scale subsets. but i want to be able to think of them as broken, proximately-oriented, melodic ‘runs’ and have that work too.
// it’s that easy
type Chord = Scale
let major = Chord([4;7;12])
let minor = Chord([3;7;12])

type Letterform = string
type Accidental =
    { delta : int
      glyph : string }
      member this.toTup() = (this.delta, this.glyph)

type Alphabet = { forms   : List<Letterform>;
                  acc     : List<Accidental>;
                  spacing : Scale }
type PitchClass =
    { name : Letterform
      acc : Accidental }
      member this.toStr() = this.name + this.acc.glyph

// ordinal step, counting from one, total size (e.g. 4 for a major 3rd), counting from zero
type Interval = { step : int; size: int }


let inline (>>>) (abc : Alphabet) (n : int) =
    let xyz : Alphabet = { forms = (abc.forms >@> n + 1);
                          acc = (abc.acc >@> n + 1);
                          spacing = (abc.spacing.mode(n)) }
    xyz

let reDup str n = ("", List.replicate n str) ||> List.fold (fun s v -> s + v)

let DblFlat  : Accidental = { delta = -2; glyph = "𝄫" }
let Flat     : Accidental = { delta = -1; glyph = "♭" }
let Natural  : Accidental = { delta =  0; glyph = "♮" }
let Sharp    : Accidental = { delta =  1; glyph = "♯" }
let DblSharp : Accidental = { delta =  2; glyph = "𝄪" }

let angloAcc = [DblFlat;Flat;Natural;Sharp;DblSharp]

let anglo : Alphabet = {forms = [for c in 'A'..'G' -> string c];
                        acc = angloAcc;
                        spacing = (diatonic.mode(6))}
let german : Alphabet = {forms = ["A";"B";"H";"C";"D";"E";"F";"G"];
                         acc = angloAcc;
                         spacing = diatonic.mode(6) + 4}
let solfege : Alphabet = {forms = ["do";"re";"mi";"fa";"sol";"la";"ti"];
                          acc = angloAcc;
                          spacing = diatonic}
let hindustani : Alphabet = {forms = ["सा";"रे";"ग";"म";"प";"ध";"नि"];
                             acc = [];
                             spacing = diatonic}

type Quality = Major | Minor
type Tonality = Alphabet * PitchClass * Scale
type Key = PitchClass * Quality

// produces the PitchClass
let IntSpelling (from : PitchClass) (interval : Interval) (abc : Alphabet) =
    let i = List.findIndex (fun x -> x = from.name) abc.forms
    let gap = interval.step
    let j = gap + i
//    let m = (abc.spacing >>> i |> Set.toList)[..gap]
    ((abc.forms >@> j)[0], i, j, gap)

//let Distance (a : PitchClass) (b : Pitch Class) (abc : Alphabet) : int =


// TO-DO: make sure there’s a fallback for spamming accidentals, 3♯/♭ &c.
let KeyMapping (tonic : PitchClass) (s : Scale) (abc : Alphabet) =
    // cycle through both `a`’s List<Letterform> and `abc`’s Scale until the first
    // element of the former matches `tonic.fst`
    // if `tonic` has an accidental, set it to all notes of the scale
    //
    // compare `s` to `abc`’s Scale, use difference to decide further accidentals
    ////// e.g. D♭ major ⇥ [D♭;E♭;F♮;G♭;A♭;B♭;C♮] ∵
    ////// D♭_Dorian = A_Minor >>> 4 ||> ♭ = [D♭;E♭;F♭;G♭;A♭;B♭;C♭]⸻
    ////// dorian[2] = 3 < diatonic[2] = 4
    let l = s.lst
    let pivot = List.findIndex (fun x -> x = tonic.name) abc.forms // pos of tonic in alphabet used
//    let xyz = abcMode abc pivot
    let delta = tonic.acc.delta // accidental of root note
    0

    //[ for i in 0..(Card s - 1) -> ] <@< 1

//let ContainsSubset (x : Scale) (y : Scale) =
    // etc
// TO-DO: reimplenent in terms of count-from-oneordinal interval №s
let RootTriad (s : Scale) = Set.empty.Add(s %> 3).Add(s %> 5).Add(s.period) |> Set.toList |> Scale

let Triads (s : Scale) = [for m in (Modes s) -> RootTriad m]
