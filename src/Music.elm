module Music exposing (..)

import Array
import List.Extra exposing (scanl)
import Regex as Regex



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


chordPitches : Octave -> Chord -> Maybe (List Pitch)
chordPitches octave { root, intervals } =
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


chordPitchClasses : Chord -> Maybe (List PitchClass)
chordPitchClasses chord =
    case chordPitches 4 chord of
        Nothing ->
            Nothing

        Just pcs ->
            Just <| List.map .pitchClass pcs


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



-- TODO: should this be in a PitchClass module so we can do
-- `PitchClass.fromString`? (and maybe `PitchClass.toInt`)


chordNameRegex : Regex.Regex
chordNameRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "([ABCDEFG]{1}[#b]{0,2})(.*)"


chordFromString : String -> Maybe Chord
chordFromString s =
    let
        components matches =
            case matches of
                Nothing ->
                    Nothing

                Just (pc :: cd :: _) ->
                    Just
                        ( pcFromString (Maybe.withDefault "" pc)
                        , chordDefinitionFromString (Maybe.withDefault "" cd)
                        )

                Just _ ->
                    Nothing

        buildChord cs =
            case cs of
                ( Nothing, _ ) ->
                    Nothing

                ( _, Nothing ) ->
                    Nothing

                ( Just pc, Just cd ) ->
                    Just <| Chord pc cd
    in
    Regex.find chordNameRegex s
        |> List.map .submatches
        |> List.head
        |> components
        |> Maybe.andThen buildChord


chordToString : Octave -> Chord -> String
chordToString octave chord =
    case chordPitches octave chord of
        Nothing ->
            ""

        Just pcs ->
            List.map pitchToString pcs |> String.join " "


pitchToString : Pitch -> String
pitchToString { pitchClass, octave } =
    pcToString pitchClass ++ String.fromInt octave


basicChordPitches =
    chordPitches 4


pcFromString : String -> Maybe PitchClass
pcFromString s =
    case s of
        "Cbb" ->
            Just Cff

        "Cb" ->
            Just Cf

        "C" ->
            Just C

        "Dbb" ->
            Just Dff

        "C#" ->
            Just Cs

        "Db" ->
            Just Df

        "C##" ->
            Just Css

        "D" ->
            Just D

        "Ebb" ->
            Just Eff

        "D#" ->
            Just Ds

        "Eb" ->
            Just Ef

        "Fbb" ->
            Just Fff

        "D##" ->
            Just Dss

        "E" ->
            Just E

        "Fb" ->
            Just Ff

        "E#" ->
            Just Es

        "F" ->
            Just F

        "Gbb" ->
            Just Gff

        "E##" ->
            Just Ess

        "F#" ->
            Just Fs

        "Gb" ->
            Just Gf

        "F##" ->
            Just Fss

        "G" ->
            Just G

        "Abb" ->
            Just Aff

        "G#" ->
            Just Gs

        "Ab" ->
            Just Af

        "G##" ->
            Just Gss

        "A" ->
            Just A

        "Bbb" ->
            Just Bff

        "A#" ->
            Just As

        "Bb" ->
            Just Bf

        "A##" ->
            Just Ass

        "B" ->
            Just B

        "B#" ->
            Just Bs

        "B##" ->
            Just Bss

        _ ->
            Nothing


pcToString : PitchClass -> String
pcToString pc =
    case pc of
        Cff ->
            "Cbb"

        Cf ->
            "Cb"

        C ->
            "C"

        Dff ->
            "Dbb"

        Cs ->
            "C#"

        Df ->
            "Db"

        Css ->
            "C##"

        D ->
            "D"

        Eff ->
            "Ebb"

        Ds ->
            "D#"

        Ef ->
            "Eb"

        Fff ->
            "Fbb"

        Dss ->
            "D##"

        E ->
            "E"

        Ff ->
            "Fb"

        Es ->
            "E#"

        F ->
            "F"

        Gff ->
            "Gbb"

        Ess ->
            "E##"

        Fs ->
            "F#"

        Gf ->
            "Gb"

        Fss ->
            "F##"

        G ->
            "G"

        Aff ->
            "Abb"

        Gs ->
            "G#"

        Af ->
            "Ab"

        Gss ->
            "G##"

        A ->
            "A"

        Bff ->
            "Bbb"

        As ->
            "A#"

        Bf ->
            "Bb"

        Ass ->
            "A##"

        B ->
            "B"

        Bs ->
            "B#"

        Bss ->
            "B##"


chordDefinitionFromString : String -> Maybe ChordDefinition
chordDefinitionFromString s =
    case s of
        "" ->
            Just majorTriad

        "sus2" ->
            Just majorSus2

        "sus4" ->
            Just majorSus4

        "-" ->
            Just minorTriad

        "0" ->
            Just diminishedTriad

        "°" ->
            Just diminishedTriad

        "+" ->
            Just augmentedTriad

        "Maj7" ->
            Just majorSeventhChord

        "MA7" ->
            Just majorSeventhChord

        "6" ->
            Just majorSixthChord

        "7" ->
            Just dominantSeventhChord

        "7sus4" ->
            Just dominantSeventhSus4Chord

        "-7" ->
            Just minorSeventhChord

        "-7b5" ->
            Just minorSeventhFlat5Chord

        "ø7" ->
            Just minorSeventhFlat5Chord

        -- half-diminished seventh
        "+7" ->
            Just augmentedSeventhChord

        "7b5" ->
            Just dominantSeventhFlat5Chord

        "-Maj7" ->
            Just minorMajorChord

        "07" ->
            Just diminishedSeventhChord

        -- fully diminished seventh
        "°7" ->
            Just diminishedSeventhChord

        "-6" ->
            Just minorSixthChord

        _ ->
            Nothing
