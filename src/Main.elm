module Main exposing (Document, Model, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { chordString = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "Chordata"
    , body =
        [ div [ id "chord-display" ] [ text "Hello world" ]
        ]
    }
