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

let reDup str n = ("", List.replicate n str) ||> List.fold (fun s v -> s + v)

let inline (>@>) (lst : List<'a>) (n : int) : List<'a> =
    let index = (n % lst.Length)
    match index with
        | 0 -> lst
        | _ -> List.skip index lst @ List.take index lst
let inline (<@<) lst (n : int) = lst >@> lst.Length - n

let ReSeat x offset modulus = // determines the relationship of `offset` to `x` (mod `modulus`)
    let diff = x - offset
    match diff > 0 with
        | true -> diff
        | false -> diff + modulus

type Scale(lst : List<int>) =
    member this.set = Set.ofList lst
    member this.lst = Set.toList this.set
    member this.card = this.set.Count
    member this.period = this.set.MaximumElement

    // a unique, *dense* binary encoding for every scale of any cardinality, as an integer
    member this.bin = [ for n in this.set -> 2. ** float (n - 1) ] |> List.sum |> int
    member this.bitfield = Convert.ToString(this.bin, 2).ToCharArray() |> Array.map(fun x -> int x - int '0') |> Array.rev |> Array.toList

    // modulates a scale; returns the nth mode of a scale (counting from zero, aeolian is `diatonic |> mode 6`); inverts chords
    static member (>>>) (s : Scale, n : int) : Scale =
        let offset = n % s.card
        if offset <> 0 then
            let new_root = s.lst[offset - 1]
            Scale(s.lst |> List.map(fun x -> ReSeat x new_root s.period))
        else s
    static member (<<<) (s : Scale, n : int) = s >>> s.card - n
    member this.mode (n : int) = Scale(this.lst) >>> n - 1

    // specific locations
    static member (%.) (s : Scale, n : int) = s.lst[abs(n % s.card)]
    static member (%>) (s : Scale, n : int) = s %. n - 2 // music counts from 1; make sure this works with Cycle and Fragment
    static member steps (s : Scale) = [ for i in 0..s.card-1 -> abs(s %. i - s %. (i - 1))]

    // editing scales
    static member (+) (s : Scale, n : int)   = Scale(s.lst @ [n])
    static member (+) (s : Scale, z : Scale) = Scale(s.lst @ z.lst)

    new() = Scale([0])

let (|Cycle|Fragment|) (s : Scale) = if s.set.MinimumElement = 0 then Fragment else Cycle

let Modes(s : Scale) = [ for i in 1..s.card -> s.mode(i) ]

(***
        CHORDS
***)

// i think it’s usually best to work with chords as full scales/treat them cyclically, because they are invertible and octave-displacable/create a harmonic space as scale subsets. but i want to be able to think of them as broken, proximately-oriented, melodic ‘runs’ and have that work too.
// it’s that easy
type Chord = Scale

type Letterform = string
type Accidental = // TODO : accidental access function
    { delta : int; glyph : string }
    member this.tup() = (this.delta, this.glyph)

type PitchClass =
    { name : Letterform; acc : Accidental }
    member this.s : string = this.name + this.acc.glyph

type Interval = { step : int; size: int }

type Alphabet(glyphs  : List<Letterform>,
              accs    : List<Accidental>,
              spacing : Scale) =
    member this.accs = accs
    member this.accmap : Map<int, Accidental> = [ for x in this.accs -> (x.delta, x) ] |> Map.ofList
    member this.acc (d : int) : Accidental =
        if this.accmap.ContainsKey d then
            this.accmap[d]
        else
            { delta = d; glyph = (reDup (this.accmap[d / abs(d)].glyph) (abs(d))) }
    member this.glyphs = glyphs
    member this.spacing = spacing
    member this.card = spacing.card

    static member (>>>) (abc : Alphabet, n : int) =
        Alphabet(abc.glyphs >@> n, abc.accs >@> n, abc.spacing.mode(n + 1))
    static member (<<<) (abc : Alphabet, n : int) = abc >>> abc.card - n

type Quality = Major | Minor
type Tonality = Alphabet * PitchClass * Scale
type Key = PitchClass * Quality

let Spelling (root : PitchClass) (ivl : Interval) (a : Alphabet) : PitchClass =
    let root_i = List.findIndex (fun x -> x = root.name) a.glyphs
    let abc = a >>> root_i

    let res_i = root_i + ivl.step - 1
    let xyz = a >>> res_i

    let delta = root.acc.delta - ((abc.spacing.lst[ivl.step-2]) - ivl.size)

    let res_acc = a.acc(delta)
    let res : PitchClass = {name = xyz.glyphs[0]; acc = res_acc}
    printfn "a %A up from %A is %A" (ivl.step, ivl.size) root.s res.s
    res

// TO-DO: make sure there’s a fallback for spamming accidentals, 3♯/♭ &c.
let KeyMapping (tonic : PitchClass) (s : Scale) (abc : Alphabet) =
    // cycle through both `a`’s List<Letterform> and `abc`’s Scale until the first
    // element of the former matches `tonic.fst`
    // if `tonic` has an accidental, set it to all notes of the scale
    // compare `s` to `abc`’s Scale, use difference to decide further accidentals
    ////// e.g. D♭ major ⇥ [D♭;E♭;F♮;G♭;A♭;B♭;C♮] ∵
    ////// D♭_Dorian = A_Minor >>> 4 ||> ♭ = [D♭;E♭;F♭;G♭;A♭;B♭;C♭]⸻
    ////// dorian[2] = 3 < diatonic[2] = 4
    let root_i = List.findIndex (fun x -> x = tonic.name) abc.glyphs // pos of tonic in alphabet used
    let delta = tonic.acc.delta // accidental of root note
    let xyz = abc >>> root_i
    xyz

let RootTriad (s : Scale) = [ s %> 3; s %> 5; s.period ] |> Scale
let Triads (s : Scale) = [ for m in (Modes s) -> RootTriad m ]

// TO-DO
// let Distance (a : PitchClass) (b : PitchClass) (abc : Alphabet) : int =
// let ForteNo
// let ContainsSubset

(***
DEFS
***)

let chromatic = Scale([1..12]) // root omitted (i.e. Cycle as above); last element is the period/modulus (e.g. the octave, conventionally) of the scale
let diatonic  = Scale([2;4;5;7;9;11;12])
let pent      = Scale([2;4;7;9;12])
let fragHWH   = Scale([0;1;3;4]) // hmm.

let ionian     = diatonic.mode(1)
let dorian     = diatonic.mode(2)
let phrygian   = diatonic.mode(3)
let lydian     = diatonic.mode(4)
let mixolydian = diatonic.mode(5)
let aeolian    = diatonic.mode(6)
let locrian    = diatonic.mode(7)

let major = Chord([4;7;12])
let minor = Chord([3;7;12])

let DblFlat  : Accidental = { delta = -2; glyph = "𝄫" }
let Flat     : Accidental = { delta = -1; glyph = "♭" }
let Natural  : Accidental = { delta =  0; glyph = "♮" }
let Sharp    : Accidental = { delta =  1; glyph = "♯" }
let DblSharp : Accidental = { delta =  2; glyph = "𝄪" }
let angloAcc = [DblFlat;Flat;Natural;Sharp;DblSharp]

let anglo      = Alphabet([for c in 'A'..'G' -> string c],       angloAcc, diatonic.mode(6))
let german     = Alphabet(["A";"B";"H";"C";"D";"E";"F";"G"],     angloAcc, diatonic.mode(6) + 4)
let solfege    = Alphabet(["do";"re";"mi";"fa";"sol";"la";"ti"], angloAcc, diatonic)
let hindustani = Alphabet(["सा";"रे";"ग";"म";"प";"ध";"नि"],        [],        diatonic)
