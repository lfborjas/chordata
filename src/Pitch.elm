module Pitch exposing (Pitch, Octave, AbsPitch, pitch, absPitch)

import Array
import PitchClass as PitchClass exposing (PitchClass, ScaleTone(..), Accidental(..))
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

{- |return the MIDI note value corresponding to a note,
   e.g.
    absPitch <| Pitch C 4
    > 60
 -}
absPitch : Pitch -> AbsPitch
absPitch { pitchClass, octave } =
    12 * (octave + 1) + PitchClass.toInt pitchClass


pitch : AbsPitch -> Maybe Pitch
pitch ap =
    let
        oct =
            ap // 12

        n =
            modBy 12 ap

        defaults =
            [ PitchClass C Natural
            , PitchClass C Sharp
            , PitchClass D Natural
            , PitchClass D Sharp
            , PitchClass E Natural
            , PitchClass F Natural
            , PitchClass F Sharp
            , PitchClass G Natural
            , PitchClass G Sharp
            , PitchClass A Natural
            , PitchClass A Sharp
            , PitchClass B Natural
            ]

        pc =
            Array.get n (Array.fromList defaults)
    in
    Maybe.map (\v -> {pitchClass = v, octave = oct - 1}) pc 