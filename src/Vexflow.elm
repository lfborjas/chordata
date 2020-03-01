port module Vexflow exposing (singleChord)

import Json.Encode as E


port singleChord : E.Value -> Cmd msg
