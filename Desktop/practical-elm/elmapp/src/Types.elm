module Types exposing (..)

import Auth exposing (Model)

type alias AppState =
    { auth : Auth.Model
    , currPlanText : String
    , isMenuOpen : Bool
    , lastError : String
    , serverUrl : String
    }