module Chord exposing (..)

import Regex
import Result
import PitchClass exposing (PitchClass)
import Interval exposing (Interval(..), intervalLength)
import Pitch exposing (Pitch, Octave, AbsPitch, absPitch, pitch)
import Scale exposing (..)
-- from my other project:
-- https://github.com/lfborjas/piano-pal/blob/6d8d0067d9a14c6a7339a2517d73dd3bac15373e/src/Scales.hs

type alias ChordDefinition =
    List Interval


type alias Chord =
    { root : PitchClass
    , intervals : ChordDefinition
    }

{-| Get the pitches in a chord:

   > chord 4 (Chord C major) 4
   Just [{ octave = 4, pitchClass = C }
        ,{ octave = 4, pitchClass = E }
        ,{ octave = 4, pitchClass = G }
        ] : Maybe (List Pitch)

-}
pitches : Octave -> Chord -> Maybe (List Pitch)
pitches octave { root, intervals } =
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

pitchClasses : Chord -> List PitchClass
pitchClasses {root, intervals} = 
    List.filterMap (applyInterval root >> Result.toMaybe) intervals

basic : Chord -> Maybe (List Pitch)
basic = pitches 4

addInterval : AbsPitch -> Interval -> AbsPitch
addInterval ap interval =
    let
        len =
            intervalLength interval
    in
    ap + len


chordNameRegex : Regex.Regex
chordNameRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "([ABCDEFG]{1}[#b]{0,2})(.*)"


fromString : String -> Maybe Chord
fromString s =
    let
        components matches =
            case matches of
                Nothing ->
                    Nothing

                Just (pc :: cd :: _) ->
                    Just
                        ( PitchClass.fromString (Maybe.withDefault "" pc)
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


pitchToString : Pitch -> String
pitchToString { pitchClass, octave } =
    PitchClass.toString pitchClass ++ String.fromInt octave



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


majorTriad : ChordDefinition
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

