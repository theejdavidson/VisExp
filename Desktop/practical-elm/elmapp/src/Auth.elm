module Auth exposing (Model, Msg(..), init, update)

import Http
import Json.Decode
import Json.Encode
import Ports exposing (..)
import Time
import Utils exposing (..)
import Types exposing (..)

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
    , sessionId : Maybe SessionId
    , userName : String
    }

init : Maybe SessionId -> Model
init sessionId =
    { lastError = ""
    , password = ""
    , sessionId = sessionId
    , userName = ""
    }

update : String -> Msg -> Model -> ( Model, Cmd Msg )
update serverUrl msg model =
    case msg of
        ChangePassword p ->
            ( { model | password = p }, Cmd.none )

        ChangeUserName name ->
            ( { model | userName = name }, Cmd.none )

        FinishLogin (Ok sessionIdStr) ->
            ( { model
                | sessionId = makeSessionId sessionIdStr
                , userName = ""
                , password = ""
              }
            , saveSessionId <| Just sessionIdStr
            )

        FinishLogin (Err error) ->
            ( { model | lastError = httpErrorString error }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        SendHeartbeat _ ->
            case model.sessionId of
                Just sessionId ->
                    ( model, sendHeartbeat serverUrl sessionId )

                Nothing ->
                    ( model, Cmd.none )

        StartLogin ->
            ( model, login serverUrl model.userName model.password )
sendHeartbeat : String -> SessionId -> Cmd Msg
sendHeartbeat serverUrl sessionId =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "SessionId" <| asString sessionId ]
        , url = serverUrl ++ "heartbeat"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectWhatever <| always NoOp
        }

login : String -> String -> String -> Cmd Msg
login serverUrl userName password =
    let
        body =
            Http.jsonBody <|
                Json.Encode.object
                    [ ( "userName", Json.Encode.string userName )
                    , ( "password", Json.Encode.string password )
                    ]

        responseDecoder =
            Json.Decode.field "sessionId" Json.Decode.string
    in
    Http.post
        { url = serverUrl ++ "login"
        , body = body
        , expect = Http.expectJson FinishLogin responseDecoder
        }
