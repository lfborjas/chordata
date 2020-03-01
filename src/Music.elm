module Music exposing (AbsPitch, Interval(..), Octave, Pitch, PitchClass(..), absPitch, defaultPCs, pcToInt, pitch)

import Array



-- from Euterpea:
-- https://github.com/Euterpea/Euterpea2/blob/55f78907ad29ce35e7e0b5ca101b60cd0efca555/Euterpea/Music.lhs


type alias AbsPitch =
    Int


type alias Octave =
    Int


type alias Pitch =
    { pitchClass : PitchClass
    , octave : Octave
    }


type alias IntervalLength =
    Int


type PitchClass
    = Cff
    | Cf
    | C
    | Dff
    | Cs
    | Df
    | Css
    | D
    | Eff
    | Ds
    | Ef
    | Fff
    | Dss
    | E
    | Ff
    | Es
    | F
    | Gff
    | Ess
    | Fs
    | Gf
    | Fss
    | G
    | Aff
    | Gs
    | Af
    | Gss
    | A
    | Bff
    | As
    | Bf
    | Ass
    | B
    | Bs
    | Bss



-- https://en.wikipedia.org/wiki/Interval_(music)#Main_intervals
-- And: https://en.wikipedia.org/wiki/Interval_(music)#Main_intervals


type Interval
    = PerfectUnison -- DiminishedSecond
    | MinorSecond -- AugmentedUnison
    | MajorSecond -- DiminishedThird
    | MinorThird -- AugmentedSecond
    | MajorThird -- DiminishedFourth
    | PerfectFourth -- AugmentedThird
    | DiminishedFifth -- AugmentedFourth
    | PerfectFifth -- DiminishedSixth
    | MinorSixth -- AugmentedFifth
    | MajorSixth -- DiminishedSeventh
    | MinorSeventh -- AugmentedSixth
    | MajorSeventh -- DiminishedOctave
    | PerfectOctave -- AugmentedSeventh | DiminishedNinth
    | MinorNinth -- AugmentedOctave
    | MajorNinth -- DiminishedTenth
    | MinorTenth -- AugmentedNinth
    | MajorTenth -- DiminishedEleventh
    | PerfectEleventh -- AugmentedTenth
    | DiminishedTwelfth -- AugmentedEleventh
    | PerfectTwelfth -- Tritave | DiminishedThirteenth
    | MinorThirteenth -- AugmentedTwelfth
    | MajorThirteenth -- DiminishedFourteenth
    | MinorFourteenth -- AugmentedThirteenth
    | MajorFourteenth -- DiminishedFifteenth
    | DoubleOctave -- PerfectFifteenth | AugmentedFourteenth


defaultPCs : List PitchClass
defaultPCs =
    [ C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B ]



-- return the MIDI note value corresponding to a note,
-- e.g.
-- absPitch <| Pitch C 4
-- > 60


absPitch : Pitch -> AbsPitch
absPitch { pitchClass, octave } =
    12 * (octave + 1) + pcToInt pitchClass


pitch : AbsPitch -> Maybe Pitch
pitch ap =
    let
        oct =
            ap // 12

        n =
            modBy 12 ap

        pc =
            Array.get n (Array.fromList defaultPCs)
    in
    case pc of
        Nothing ->
            Nothing

        Just v ->
            Just { pitchClass = v, octave = oct - 1 }


pcToInt : PitchClass -> Int
pcToInt pc =
    case pc of
        Cff ->
            -2

        Cf ->
            -1

        C ->
            0

        Cs ->
            1

        Css ->
            2

        Dff ->
            0

        Df ->
            1

        D ->
            2

        Ds ->
            3

        Dss ->
            4

        Eff ->
            2

        Ef ->
            3

        E ->
            4

        Es ->
            5

        Ess ->
            6

        Fff ->
            3

        Ff ->
            4

        F ->
            5

        Fs ->
            6

        Fss ->
            7

        Gff ->
            5

        Gf ->
            6

        G ->
            7

        Gs ->
            8

        Gss ->
            9

        Aff ->
            7

        Af ->
            8

        A ->
            9

        As ->
            10

        Ass ->
            11

        Bff ->
            9

        Bf ->
            10

        B ->
            11

        Bs ->
            12

        Bss ->
            13
