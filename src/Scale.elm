module Scale exposing (..)

import Array
import Interval as Interval exposing (Interval(..), intervalLength)
import List
import PitchClass as PitchClass exposing (..)


type Degree
    = I
    | II
    | III
    | IV
    | V
    | VI
    | VII


degreeToInt : Degree -> Int
degreeToInt d =
    case d of
        I ->
            0

        II ->
            1

        III ->
            2

        IV ->
            3

        V ->
            4

        VI ->
            5

        VII ->
            6


degreeForInterval : Interval -> Degree
degreeForInterval i =
    case i of
        PerfectUnison ->
            I

        MinorSecond ->
            II

        MajorSecond ->
            II

        MinorThird ->
            III

        MajorThird ->
            III

        PerfectFourth ->
            IV

        DiminishedFifth ->
            V

        PerfectFifth ->
            V

        AugmentedFifth ->
            V

        MajorSixth ->
            VI

        DiminishedSeventh ->
            VII

        MinorSeventh ->
            VII

        MajorSeventh ->
            VII

        PerfectOctave ->
            I

        MinorNinth ->
            II

        MajorNinth ->
            II

        MajorTenth ->
            III

        MinorTenth ->
            III

        PerfectEleventh ->
            IV

        DiminishedTwelfth ->
            V

        PerfectTwelfth ->
            V

        MinorThirteenth ->
            VI

        MajorThirteenth ->
            VI

        MinorFourteenth ->
            VII

        MajorFourteenth ->
            VII

        DoubleOctave ->
            VII


applyInterval : PitchClass -> Interval -> Result String PitchClass
applyInterval root interval =
    let
        defaults =
            [ C, D, E, F, G, A, B ]

        ( before, after ) =
            List.partition (\x -> toneToInt x < toneToInt root.scaleTone) defaults

        tones =
            after ++ before

        n =
            interval |> degreeForInterval |> degreeToInt

        canonicalLength =
            modBy 12 (intervalLength interval)

        tone =
            Array.get n (Array.fromList tones)

        pc =
            Maybe.map (\x -> PitchClass x Natural) tone

        distanceWhenTone =
            Maybe.map (\x -> canonicalLength - (PitchClass.toInt x - PitchClass.toInt root |> abs)) pc
    in
    case distanceWhenTone of
        Nothing ->
            Err "Unable to find right scale degree"

        Just distance ->
            case pc of
                Nothing ->
                    Err "Unable to construct pitch class (?)"

                Just pc_ ->
                    case ( distance, negate distance ) of
                        ( _, 2 ) ->
                            Ok { pc_ | accidental = DoubleFlat }

                        ( _, 1 ) ->
                            Ok { pc_ | accidental = Flat }

                        ( 0, _ ) ->
                            Ok pc_

                        ( 1, _ ) ->
                            Ok { pc_ | accidental = Sharp }

                        ( 2, _ ) ->
                            Ok { pc_ | accidental = DoubleSharp }

                        _ ->
                            Err "unable to apply interval in the desired scale degree"
