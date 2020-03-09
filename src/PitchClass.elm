module PitchClass exposing (fromString, toString, toInt, PitchClass, ScaleTone(..), Accidental(..))

import List.Extra exposing (iterate)
import Tuple

type ScaleTone 
    = A
    | B
    | C
    | D
    | E
    | F
    | G

type Accidental 
    = DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp

type alias PitchClass =
    {
        scaleTone : ScaleTone
    ,   accidental : Accidental   
    }

{- semitones from C -}
toneToInt : ScaleTone -> Int
toneToInt t =
    case t of
        C -> 0
        D -> 2
        E -> 4
        F -> 5
        G -> 7
        A -> 9
        B -> 11

toneToString : ScaleTone -> String
toneToString t =
    case t of
        C -> "C"
        D -> "D"
        E -> "E"
        F -> "F"
        G -> "G"
        A -> "A"
        B -> "B"
            
    

accidentalToInt : Accidental -> Int
accidentalToInt a =
    case a of
        DoubleFlat -> -2
        Flat -> -1
        Natural -> 0
        Sharp -> 1
        DoubleSharp -> 2

accidentalToString : Accidental -> String
accidentalToString a =
    case a of
        DoubleFlat -> "bb"
        Flat -> "b"
        Natural -> ""
        Sharp -> "#"
        DoubleSharp -> "##"
    

toInt : PitchClass -> Int
toInt {scaleTone, accidental} = 
    let
        s = toneToInt scaleTone
        a = accidentalToInt accidental
    in
    s + a

toString : PitchClass -> String
toString {scaleTone, accidental} =
    let
        s = toneToString scaleTone
        a = accidentalToString accidental
    in
    s ++ a

sharpen : PitchClass -> Result String PitchClass
sharpen pc =
    case pc.accidental of
        DoubleFlat -> Ok {pc | accidental = Flat}
        Flat -> Ok {pc | accidental = Natural}    
        Natural -> Ok {pc | accidental = Sharp}
        Sharp -> Ok {pc | accidental = DoubleSharp}
        _     -> Err "Can't sharpen tone without moving pitch class!"

toneFromString : String -> Result String ScaleTone
toneFromString s =
    case s of
       "C" -> Ok C
       "D" -> Ok D
       "E" -> Ok E
       "F" -> Ok F
       "G" -> Ok G
       "A" -> Ok A
       "B" -> Ok B
       _   -> Err "Unknown scale tone"

accidentalFromString : String -> Result String Accidental
accidentalFromString s =
    case s of
        "bb" -> Ok DoubleFlat
        "b"  -> Ok Flat
        ""   -> Ok Natural
        "#"  -> Ok Sharp
        "##" -> Ok DoubleSharp
        _    -> Err "Unknown accidental"

-- meh: have to muck with either parsers or more regex chicanery
-- fromStringR : String -> Result String PitchClass
-- fromStringR s =


-- TODO! this can be formed by doing a Maybe.andThen from fromStrings functions for both st and accidental!!
fromString : String -> Maybe PitchClass
fromString s =
    case s of
        "Cbb" ->
            Just <| PitchClass C DoubleFlat

        "Cb" ->
            Just <| PitchClass C Flat

        "C" ->
            Just <| PitchClass C Natural

        "Dbb" ->
            Just <| PitchClass D DoubleFlat

        "C#" ->
            Just <| PitchClass C Sharp

        "Db" ->
            Just <| PitchClass D Flat

        "C##" ->
            Just <| PitchClass C DoubleSharp

        "D" ->
            Just <| PitchClass D Natural

        "Ebb" ->
            Just <| PitchClass E DoubleFlat

        "D#" ->
            Just <| PitchClass D Sharp

        "Eb" ->
            Just <| PitchClass E Flat

        "Fbb" ->
            Just <| PitchClass F DoubleFlat

        "D##" ->
            Just <| PitchClass D DoubleSharp

        "E" ->
            Just <| PitchClass E Natural

        "Fb" ->
            Just <| PitchClass F Flat

        "E#" ->
            Just <| PitchClass E Sharp

        "F" ->
            Just <| PitchClass F Natural

        "Gbb" ->
            Just <| PitchClass G DoubleFlat

        "E##" ->
            Just <| PitchClass E DoubleSharp

        "F#" ->
            Just <| PitchClass F Sharp

        "Gb" ->
            Just <| PitchClass G Flat

        "F##" ->
            Just <| PitchClass F DoubleSharp

        "G" ->
            Just <| PitchClass G Natural

        "Abb" ->
            Just <| PitchClass A DoubleFlat

        "G#" ->
            Just <| PitchClass G Sharp

        "Ab" ->
            Just <| PitchClass A Flat

        "G##" ->
            Just <| PitchClass G DoubleSharp

        "A" ->
            Just <| PitchClass A Natural

        "Bbb" ->
            Just <| PitchClass B DoubleFlat

        "A#" ->
            Just <| PitchClass A Sharp

        "Bb" ->
            Just <| PitchClass B Flat

        "A##" ->
            Just <| PitchClass A DoubleSharp

        "B" ->
            Just <| PitchClass B Natural

        "B#" ->
            Just <| PitchClass B Sharp

        "B##" ->
            Just <| PitchClass B DoubleSharp

        _ ->
            Nothing