module Pages.Registration exposing (Model, Msg(..), Platform(..), init)

import Http
import Types exposing (..)
import Json.Decode
import Json.Encode
import Ports exposing (..)
import Element exposing (..)
import Element.Input as Input
import Attr exposing (..)
import Utils exposing (httpErrorString)
type Msg
    = ChangePassword String
    | ChangeRepeatPassword String
    | ChangeUserName String
    | FinishRegistration (Result Http.Error String)
    | SelectPlatform Platform
    | StartRegistration
    | ToggleAcceptTerms Bool

type PageMsg
    = DoNothing
    | FinishSuccessfully String

type Platform
    = Aws
    | Azure
    | Heroku
    | SelfHosted

type alias Model =
    { errors : List String
    , hasAcceptedTerms : Bool
    , password : String
    , platform : Maybe Platform
    , repeatPassword : String
    , userName : String
    }

type alias AppStateSubset a =
    { a | serverUrl :String, sessionId : Maybe String }

init : Model
init =
    { hasAcceptedTerms = False
    , errors = []
    , password = ""
    , platform = Nothing
    , repeatPassword = ""
    , userName = ""
    }

update : Msg -> { a | serverUrl : String } -> Model -> ( Model, Cmd Msg, PageMsg )
update msg { serverUrl} model =
    case msg of
        ChangePassword p ->
            ( { model | password = p }, Cmd.none, DoNothing )
        
        ChangeRepeatPassword p ->
            ( { model | repeatPassword = p }, Cmd.none, DoNothing )

        ChangeUserName u ->
            ( { model | userName = u }, Cmd.none, DoNothing )

        FinishRegistration (Ok sessionIdStr) ->
            ( model
            , saveSessionId <| Just sessionId
            , FinishSuccessfully sessionIdStr
            )

        FinishRegistration (Err error) ->
            ( { model | errors = [ httpErrorString error ] }, Cmd.none, DoNothing )

        SelectPlatform platform ->
            ( { model | platform = Just platform }
            , Cmd.none
            , DoNothing
            )

        StartRegistration ->
            ( model
            , register serverUrl model.userName model.password
            , DoNothing
            )

        ToggleAcceptTerms val ->
            ( { model | hasAcceptedTerms = val }, Cmd.none, DoNothing )

register : String -> String -> String -> Cmd Msg
register serverUrl userName password =
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
        { url = serverUrl ++ "register"
        , body = body
        , expect = Http.expectJson FinishRegistration responseDecoder
        }

page : Model -> Element Msg
page model =
    column
        [ paddingXY 0 20, spacingXY 0 10, width (px 300), centerX ]
    <|
        [ Input.email Attr.input
            { onChange = ChangeUserName
            , text = model.userName
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Email:"
            }
        , Input.newPassword Attr.input
            { onChange = ChangePassword
            , text = model.password
            , placeholder = Nothing
            , show = False
            , label = Input.labelAbove [] <| text "Password:"
            }
        , Input.newPassword Attr.input
            { onChange = ChangeRepeatPassword
            , text = model.repeatPassword
            , placeholder = Nothing
            , show = False
            , label = Input.labelAbove [] <| text "Repeat password:"
            }
        , Input.radio
            [ padding 3, spacing 5]
            { onChange = SelectPlatform
            , selected = model.platform
            , label = Input.labelAbove [] <| text "Platform:"
            , options =
                [ Input.option Aws <| text "AWS"
                , Input.option Azure <| text "Azure"
                , Input.option Heroku <| text "Heroku"
                , Input.option SelfHosted <| text "Self-hosted"
                ]
            }
        , Input.checkbox
            [ padding 3]
            { onChange = ToggleAcceptTerms
            , checked = model.hasAcceptedTerms
            , label = Input.labelRight [] <| text "I accept the terms"
            , icon = Input.defaultCheckbox
            }
        , Input.button Attr.greenButton
            { onPress = Just StartRegistration
            , label = el [ centerX ] <| text "Register"
            } 
        ]
            ++ List.map (text >> el Attr.error) model.errors