module Main exposing (Document, Model, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Chord as Chord
import Vexflow exposing (singleChord)


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { chordString : Maybe String }


type Msg
    = Typed String
    | Render


init : () -> ( Model, Cmd Msg )
init _ =
    ( { chordString = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Typed newChordString ->
            ( { model | chordString = Just newChordString }, Cmd.none )

        Render ->
            ( model, encodeChord model |> singleChord )


encodeChord : Model -> Encode.Value
encodeChord { chordString } =
    Encode.object
        [ ( "elementId", Encode.string "chord-display" )
        , ( "notes", parseChordPitches chordString |> Encode.list Encode.string )
        ]


parseChordPitches : Maybe String -> List String
parseChordPitches s =
    Maybe.withDefault "" s
        |> Chord.fromString
        |> Maybe.andThen Chord.basic
        |> Maybe.withDefault []
        |> List.map Chord.pitchToString


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "Chordata"
    , body =
        -- https://tachyons.io/components/layout/centered-container/index.html
        [ section [ class "mw5 mw7-ns center pa3 ph5-ns sans-serif" ]
            -- https://tachyons.io/components/forms/newsletter-subscription/index.html
            [ fieldset [ class "ba b--transparent ph0 mh0" ]
                [ div [ class "mt3" ]
                    [ input
                        [ type_ "text"
                        , class "pa2 input-reset ba bg-transparent w-100 measure"
                        , placeholder "Type a chord"
                        , value (Maybe.withDefault "" model.chordString)
                        , onInput Typed
                        ]
                        []
                    ]
                , div [ class "mt3" ]
                    [ a
                        [ onClick Render
                        , class "f6 link dim ba bw2 ph3 pv2 mb2 dib purple"
                        ]
                        [ text "Render!" ]
                    ]
                ]
            , div [ id "chord-display" ] []
            ]
        ]
    }
