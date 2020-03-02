module Interval exposing (Interval(..), IntervalLength, intervalLength)


type alias IntervalLength =
    Int


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
