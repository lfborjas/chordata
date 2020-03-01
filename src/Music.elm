module Music exposing (AbsPitch, Chord, ChordDefinition, Interval(..), IntervalLength, Octave, Pitch, PitchClass(..), absPitch, augmentedSeventhChord, augmentedTriad, chord, defaultPCs, diminishedSeventhChord, diminishedTriad, dominantSeventhChord, dominantSeventhFlat5Chord, dominantSeventhSus4Chord, intervalLength, majorSeventhChord, majorSixthChord, majorSus2, majorSus4, majorTriad, minorMajorChord, minorSeventhChord, minorSeventhFlat5Chord, minorSixthChord, minorTriad, pcToInt, pitch)

import Array
import List.Extra exposing (scanl)



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



-- from my other project:
-- https://github.com/lfborjas/piano-pal/blob/6d8d0067d9a14c6a7339a2517d73dd3bac15373e/src/Scales.hs


type alias IntervalLength =
    Int


type alias ChordDefinition =
    List Interval


type alias Chord =
    { root : PitchClass
    , intervals : ChordDefinition
    }


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
    | AugmentedFifth -- MinorSixth
    | MajorSixth -- Same as DiminishedSeventh, but both used in Jazz on their own.
    | DiminishedSeventh -- Same as MajorSixth
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



{- Get the pitches in a chord:

   > chord (Chord C majorTriad) 4
   Just [{ octave = 4, pitchClass = C }
        ,{ octave = 4, pitchClass = E }
        ,{ octave = 4, pitchClass = G }
        ] : Maybe (List Pitch)

-}


chord : Chord -> Octave -> Maybe (List Pitch)
chord { root, intervals } octave =
    let
        startingAbsPitch =
            absPitch <| Pitch root octave

        resultingAbsPitches =
            List.map (addInterval startingAbsPitch) intervals

        resultingPitches =
            List.filterMap pitch resultingAbsPitches
    in
    case resultingPitches of
        [] ->
            Nothing

        l ->
            Just l


addInterval : AbsPitch -> Interval -> AbsPitch
addInterval ap interval =
    let
        len =
            intervalLength interval
    in
    ap + len


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


intervalLength : Interval -> IntervalLength
intervalLength interval =
    case interval of
        PerfectUnison ->
            0

        MinorSecond ->
            1

        MajorSecond ->
            2

        MinorThird ->
            3

        MajorThird ->
            4

        PerfectFourth ->
            5

        DiminishedFifth ->
            6

        PerfectFifth ->
            7

        AugmentedFifth ->
            8

        MajorSixth ->
            9

        DiminishedSeventh ->
            9

        MinorSeventh ->
            10

        MajorSeventh ->
            11

        PerfectOctave ->
            12

        MinorNinth ->
            13

        MajorNinth ->
            14

        MinorTenth ->
            15

        MajorTenth ->
            16

        PerfectEleventh ->
            17

        DiminishedTwelfth ->
            18

        PerfectTwelfth ->
            19

        MinorThirteenth ->
            20

        MajorThirteenth ->
            21

        MinorFourteenth ->
            22

        MajorFourteenth ->
            23

        DoubleOctave ->
            24



-- CHORD DEFINITIONS
-- TRIADS


majorTriad =
    [ PerfectUnison, MajorThird, PerfectFifth ]


majorSus2 =
    [ PerfectUnison, MajorSecond, PerfectFifth ]


majorSus4 =
    [ PerfectUnison, PerfectFourth, PerfectFifth ]


minorTriad =
    [ PerfectUnison, MinorThird, PerfectFifth ]


diminishedTriad =
    [ PerfectUnison, MinorThird, DiminishedFifth ]


augmentedTriad =
    [ PerfectUnison, MajorThird, AugmentedFifth ]



-- SEVENTH AND SIXTH CHORDS


majorSeventhChord =
    majorTriad ++ [ MajorSeventh ]


majorSixthChord =
    majorTriad ++ [ MajorSixth ]


dominantSeventhChord =
    majorTriad ++ [ MinorSeventh ]


dominantSeventhSus4Chord =
    majorSus4 ++ [ MinorSeventh ]


minorSeventhChord =
    minorTriad ++ [ MinorSeventh ]


minorSeventhFlat5Chord =
    diminishedTriad ++ [ MinorSeventh ]


augmentedSeventhChord =
    augmentedTriad ++ [ MinorSeventh ]


dominantSeventhFlat5Chord =
    [ PerfectUnison, MajorThird, DiminishedFifth, MinorSeventh ]


minorMajorChord =
    minorTriad ++ [ MajorSeventh ]


diminishedSeventhChord =
    diminishedTriad ++ [ DiminishedSeventh ]


minorSixthChord =
    minorTriad ++ [ MajorSixth ]
