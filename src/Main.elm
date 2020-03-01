module Main exposing (Document, Model, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
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
        , ( "notes", String.split "," (Maybe.withDefault "" chordString) |> Encode.list Encode.string )
        ]


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "Chordata"
    , body =
        [ input
            [ type_ "text"
            , placeholder "Type a chord"
            , value (Maybe.withDefault "" model.chordString)
            , onInput Typed
            ]
            []
        , button [ onClick Render ] [ text "Render!" ]
        , div [ id "chord-display" ] []
        ]
    }
