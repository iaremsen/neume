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
    let index = n % lst.Length
    match index with
        | 0 -> lst
        | _ -> (List.skip index lst) @ (List.take index lst)
let inline (<@<) lst (n : int) = lst >@> lst.Length - n

let WrapMod x modulus =
    match x > 0 with
        | true  ->  x            % modulus
        | false -> (x + modulus) % modulus

let Reorient x offset modulus = // determines the relationship of `offset` to `x` (mod `modulus`)
    let diff = x - offset
    match diff > 0 with
        | true   -> diff
        | false  -> diff + modulus

type Scale(lst : List<int>) =
    member this.set    = Set.ofList lst
    member this.lst    = Set.toList this.set
    member this.card   = this.set.Count
    member this.period = this.set.MaximumElement

    // a unique, *dense* binary encoding for every scale of any cardinality, as an integer
    member this.bin      = [ for n in this.set -> 2. ** float (n - 1) ] |> List.sum |> int
    member this.bitfield = Convert.ToString(this.bin, 2).ToCharArray()
                           |> Array.map(fun x -> int x - int '0') |> Array.rev |> Array.toList

    // modulates a scale; returns the nth mode of a scale; inverts chords
    // counts from 0: aeolian is `diatonic >>> 5`
    static member (>>>) (s : Scale, n : int) : Scale =
        let offset = n % s.card
        if offset <> 0 then
            let new_root = s.lst[offset - 1]
            Scale(s.lst |> List.map(fun x -> Reorient x new_root s.period))
        else s
    static member (<<<) (s : Scale, n : int) = s >>> s.card - n
    // this counts from 1
    member this.mode (n : int) = Scale(this.lst) >>> n - 1

    member this.zeroPad = [0] @ this.lst <@< 1 |> List.tail

    member this.degree (d : int) =
        if d = 0 then 0
        else if d = 1 then this.period
        else if d > 1 then this.lst[(d - 2) % this.card]
        else this.degree(abs(this.card + 2 + d))

    // counts from 1, as in music
    static member (%>) (s : Scale, n : int) = s.degree(n)
    member this.steps =
        [this.lst.Head] @ [ for i in 0..this.card-2 -> abs(this.lst[i + 1] - this.lst[i]) ]

    // editing scales
    static member (+) (s : Scale, n : int)   = Scale(s.lst @ [n])
    static member (+) (s : Scale, z : Scale) = Scale(s.lst @ z.lst)

    new() = Scale([0])

let Modes(s : Scale) = [ for i in 1..s.card -> s.mode(i) ]

(***
       CHORDS
***)

// i think it’s usually best to treat chords as being full, cyclical scales b.c.
// they are invertible and octave-displacable. a 'harmonic space' can be described
// as a scale [subset].
//
// with that said, i want to be able to think of them as broken, proximately-oriented,
// melodic ‘runs’ and have that work too.
type Chord = Scale

type Letterform = string
type Accidental =
    { delta : int; glyph : string }
    member this.tup : int * string = (this.delta, this.glyph)

type PitchClass =
    { name : Letterform; acc : Accidental }
    member this.s : string = this.name + this.acc.glyph

type Interval = { step : int; size: int }

type Alphabet(glyphs  : List<Letterform>,
              accs    : Map<int, Accidental>,
              spacing : Scale) =
    member this.accs = accs
    member this.acc (d : int) : Accidental =
        if this.accs.ContainsKey d then this.accs[d]
        else { delta = d; glyph = (reDup (this.accs[d / abs(d)].glyph) (abs(d))) }
    member this.glyphs  = glyphs
    member this.spacing = spacing
    member this.card    = spacing.card
    member this.period  = spacing.period

    static member (>>>) (abc : Alphabet, n : int) = Alphabet(abc.glyphs >@> n,
                                                             abc.accs,
                                                             abc.spacing.mode(n + 1))
    static member (<<<) (abc : Alphabet, n : int) = abc >>> abc.card - n

type Quality  = Major | Minor
type Tonality = List<PitchClass>
type Key      = PitchClass * Quality

let Spelling (root : PitchClass) (ivl : Interval) (a : Alphabet) : PitchClass =
    let (m_step, m_size) = (WrapMod ivl.step a.card, WrapMod ivl.size a.period)
    let root_i = a.glyphs |> List.findIndex (fun x -> x = root.name)
    let abc    = a   >>> root_i
    let xyz    = abc >>> m_step - 1
    let delta  = root.acc.delta - (abc.spacing.degree(m_step) - m_size) % a.period
    {name = xyz.glyphs[0]; acc = a.acc(delta)}

let KeyMapping (tonic : PitchClass) (s : Scale) (abc : Alphabet) : Tonality =
    // cycle through both `s`’s List<Letterform> and `abc`’s Scale until the first
    // element of the former matches `tonic.fst`
    // if `tonic` has an accidental, set it to all notes of the scale
    // compare `s` to `abc`’s Scale, use difference to decide further accidentals
    //// e.g. D♭_ionian ≔ [D♭;E♭;F♮;G♭;A♭;B♭;C♮] ∵
    //// D♭_dorian = A_aeolian >>> 4 ||> ♭ = [D♭;E♭;F♭;G♭;A♭;B♭;C♭]⸻
    //// dorian %> 2 = 3 < ionian %> 2 = 4
    let root_i = abc.glyphs |> List.findIndex (fun x -> x = tonic.name) // pos of tonic in alphabet used
    let xyz    = abc >>> root_i
    let deltas = [ for i in 1..xyz.card -> s.degree(i) - xyz.spacing.degree(i) ]
    [ for p in 0..xyz.card-1 -> {name = xyz.glyphs[p]; acc=abc.acc(deltas[p] + tonic.acc.delta)} ]

let Triad (root : int)(s : Scale) = [ s.degree(root); s.degree(root + 2); s.degree(root + 4) ] |> Chord
let RootTriad = Triad 1
let Triads    (s : Scale) = [ for m in (Modes s) -> RootTriad m ]

// TO-DO
// let Distance (a : PitchClass) (b : PitchClass) (abc : Alphabet) : int =
// let ForteNo
// let ContainsSubset

(***
       DEFINITIONS
***)

let chromatic = Scale([1..12])
let diatonic  = Scale([2;4;5;7;9;11;12])
let pent      = Scale([2;4;7;9;12])

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
let angloAcc : Map<int, Accidental> = [DblFlat;Flat;Natural;Sharp;DblSharp]
                                      |> List.map (fun x -> (x.delta, x))
                                      |> Map.ofList

let anglo      = Alphabet([for c in 'A'..'G' -> string c],       angloAcc,  aeolian)
let german     = Alphabet(["A";"B";"H";"C";"D";"E";"F";"G"],     angloAcc,  aeolian + 4)
let solfege    = Alphabet(["do";"re";"mi";"fa";"sol";"la";"ti"], angloAcc,  diatonic)
let hindustani = Alphabet(["सा";"रे";"ग";"म";"प";"ध";"नि"],         Map.empty, diatonic)
