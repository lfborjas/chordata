module PitchClass exposing (defaults, fromString, toString, toInt, PitchClass)

defaults : List PitchClass
defaults =
    [ C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B ]

tonesFor : PitchClass -> List PitchClass
tonesFor root =
    let
        (before, after) = [C, D, E, F, G, A, B] |> List.partition (\x -> toInt x < toInt root)
        degrees         = after ++ before
        solfeges        = List.map toSolfege <| degrees
        absPitches      = iterate ((+) 1) (toInt root) |> List.take 12
    in

fromSolfege : Solfege -> List PitchClass
fromSolfege s = 
    case s of
        Do -> [Cff, Cf, C, Cs, Css]
        Re -> [Dff, Df, D, Ds, Dss]
        Mi -> [Eff, Ef, E, Es, Ess]
        Fa -> [Fff, Ff, F, Fs, Fss]
        Sol -> [Gff, Gf, G, Gs, Gss]
        La -> [Aff, Af, A, As, Ass]
        Ti -> [Bff, Bf, B, Bs, Bss]

toSolfege : PitchClass -> Solfege
toSolfege pc =
    case pc of
        Cff ->
            Do

        Cf ->
            Do

        C ->
            Do

        Cs ->
            Do

        Css ->
            Do

        Dff ->
            Re

        Df ->
            Re

        D ->
            Re

        Ds ->
            Re

        Dss ->
            Re

        Eff ->
            Mi

        Ef ->
            Mi

        E ->
            Mi

        Es ->
            Mi

        Ess ->
            Mi

        Fff ->
            Fa

        Ff ->
            Fa

        F ->
            Fa

        Fs ->
            Fa

        Fss ->
            Fa

        Gff ->
            Sol

        Gf ->
            Sol

        G ->
            Sol

        Gs ->
            Sol

        Gss ->
            Sol

        Aff ->
            La

        Af ->
            La

        A ->
            La

        As ->
            La

        Ass ->
            La

        Bff ->
            Ti

        Bf ->
            Ti

        B ->
            Ti

        Bs ->
            Ti

        Bss ->
            Ti
    
    
type Solfege 
    = Do
    | Re
    | Mi
    | Fa
    | Sol
    | La
    | Ti

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

toInt : PitchClass -> Int
toInt pc =
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

toString : PitchClass -> String
toString pc =
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

fromString : String -> Maybe PitchClass
fromString s =
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