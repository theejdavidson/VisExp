module Auth exposing (Flags, Model, Msg(..), init)

import Http
import Json.Decode
import Json.Encode
import Ports exposing (..)
import Time

type Msg
    = NoOp
    | ChangePassword String
    | ChangeUserName String
    | FinishLogin (Result Http.Error String)
    | SendHeartbeat Time.Posix
    | StartLogin

type alias Model =
    { lastError : String
    , password : String
    , sessionId : Maybe String
    , userName : String
    }

type alias Flags =
    { sessionId : Maybe String }

init : Maybe String -> Model
init sessionId =
    { lastError = ""
    , password = ""
    , sessionId = sessionId
    , userName = ""
    }