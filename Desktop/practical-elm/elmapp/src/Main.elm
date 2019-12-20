module Main exposing (..)

import Attr exposing (..)
import Auth exposing (..)
import Browser
import Browser.Events exposing (onKeyPress)
import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, div, h1, img)
import Html.Attributes exposing (src)
import Http
import Json.Decode
import Json.Encode
import Pages.Display as Display
import Pages.Registration as Registration
import Pages.SavedPlans as SavedPlans
import PlanParsers.Json exposing (..)
import PlanTree exposing (..)
import Ports exposing (..)
import Time
import Types exposing (..)
import Utils exposing (..)



---- MODEL ----


type Page
    = DisplayPage Display.Model
    | InputPage
    | LoginPage
    | RegistrationPage Registration.Model
    | SavedPlansPage SavedPlans.Model


type Msg
    = NoOp
    | Auth Auth.Msg
    | ChangePlanText String
    | CreatePlan
    | Display Display.Msg
    | DumpModel ()
    | Register Registration.Msg
    | RequestLogin
    | RequestLogout
    | RequestRegistration
    | RequestSavedPlans SessionId
    | SavedPlans SavedPlans.Msg
    | SubmitPlan
    | ToggleMenu


type alias Model =
    { appState : AppState
    , currPage : Page
    }


type alias AppState =
    { auth : Auth.Model
    , currPlanText : String
    , isMenuOpen : Bool
    , lastError : String
    , serverUrl : String
    }


type alias Flags =
    { sessionId : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { appState =
            { auth = Auth.init (flags.sessionId |> Maybe.andThen makeSessionId)
            , currPlanText = ""
            , isMenuOpen = False
            , lastError = ""
            , serverUrl = "http://localhost:3000/"
            }
      , currPage = InputPage
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dumpModel DumpModel
        , Time.every (100 * 1000) <| Auth << Auth.SendHeartbeat
        , onKeyPress <| keyDecoder model
        ]


keyDecoder : Model -> Json.Decode.Decoder Msg
keyDecoder model =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "altKey" Json.Decode.bool)
        (Json.Decode.field "shiftKey" Json.Decode.bool)
        |> Json.Decode.andThen
            (\altAndShiftFlags ->
                case altAndShiftFlags of
                    ( True, True ) ->
                        Json.Decode.field "code" Json.Decode.string
                            |> Json.Decode.map (keyToMsg model)

                    _ ->
                        Json.Decode.succeed NoOp
            )


keyToMsg : Model -> String -> Msg
keyToMsg model s =
    case ( s, model.appState.auth.sessionId ) of
        ( "s", Just sessionId ) ->
            RequestSavedPlans sessionId

        ( "n", _ ) ->
            CreatePlan

        _ ->
            NoOp



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ appState } as model) =
    case ( msg, model.currPage ) of
        ( Auth authMsg, _ ) ->
            let
                ( authModel, authCmd ) =
                    Auth.update appState.serverUrl authMsg appState.auth

                currPage =
                    case authMsg of
                        Auth.FinishLogin (Ok _) ->
                            InputPage

                        _ ->
                            model.currPage
            in
            ( { model
                | appState = { appState | auth = authModel }
                , currPage = currPage
              }
            , Cmd.map Auth authCmd
            )

        ( RequestRegistration, _ ) ->
            ( { model | currPage = RegistrationPage Registration.init }, Cmd.none )

        ( Register regMsg, RegistrationPage pageModel ) ->
            let
                ( regModel, regCmd, pageMsg ) =
                    Registration.update regMsg model.appState pageModel

                newModel =
                    case pageMsg of
                        Registration.FinishSuccessfully id ->
                            let
                                auth =
                                    appState.auth
                            in
                            { appState =
                                { appState
                                    | auth =
                                        { auth | sessionId = Just id }
                                }
                            , currPage = InputPage
                            }

                        Registration.DoNothing ->
                            { model | currPage = RegistrationPage regModel }
            in
            ( newModel
            , Cmd.map Register regCmd
            )

        ( ChangePlanText s, InputPage ) ->
            ( { model | appState = { appState | currPlanText = s } }, Cmd.none )

        ( CreatePlan, _ ) ->
            ( { model
                | appState = { appState | currPlanText = "" }
                , currPage = InputPage
              }
            , Cmd.none
            )

        ( RequestLogin, _ ) ->
            ( { model | currPage = LoginPage }
            , Cmd.none
            )

        ( RequestSavedPlans sessionId, _ ) ->
            let
                ( pageModel, pageCmd ) =
                    SavedPlans.init appState.serverUrl sessionId
            in
            ( { model | currPage = SavedPlansPage pageModel }
            , Cmd.map SavedPlans pageCmd
            )

        ( SubmitPlan, InputPage ) ->
            ( { model | currPage = DisplayPage Display.init }, Cmd.none )

        ( ToggleMenu, _ ) ->
            ( { model
                | appState = { appState | isMenuOpen = not appState.isMenuOpen }
              }
            , Cmd.none
            )

        ( DumpModel (), _ ) ->
            ( Debug.log "model" model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )

---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                DisplayPage pageModel ->
                    Display.page model.appState pageModel
                        |> Element.map Display

                InputPage ->
                    inputPage model.appState

                LoginPage ->
                    loginPage model.appState.auth

                SavedPlansPage pageModel ->
                    SavedPlans.page pageModel
                        |> Element.map SavedPlans

                RegistrationPage pageModel ->
                    Registration.page pageModel
                        |> Element.map Register
    in
    { title = "VisExp"
    , body =
        [ layout [ inFront <| menuPanel model.appState ] <|
            column [ width fill, spacingXY 0 20 ]
                [ navBar
                , content
                ]
        ]
    }


inputPage : AppState -> Element Msg
inputPage appState =
    column
        [ width (px 800)
        , spacingXY 0 10
        , centerX
        ]
        [ Input.multiline
            [ height (px 300)
            , Border.width 1
            , Border.rounded 3
            , Border.color lightCharcoal
            , padding 3
            ]
            { onChange = ChangePlanText
            , text = appState.currPlanText
            , placeholder = Nothing
            , label =
                Input.labelAbove [] <|
                    text "Paste the EXPLAIN output in JSON format:"
            , spellcheck = False
            }
        , Input.button
            (Attr.greenButton ++ [ width (px 200), height (px 40), alignRight ])
            { onPress = Just SubmitPlan
            , label = el [ centerX ] <| text "Go!"
            }
        ]


loginPage : Auth.Model -> Element Msg
loginPage model =
    column [ paddingXY 0 20, spacingXY 0 10, width (px 300), centerX ]
        [ Input.username Attr.input
            { onChange = Auth << Auth.ChangeUserName
            , text = model.userName
            , label = Input.labelAbove [] <| text "User name:"
            , placeholder = Nothing
            }
        , Input.currentPassword Attr.input
            { onChange = Auth << Auth.ChangePassword
            , text = model.password
            , label = Input.labelAbove [] <| text "Password:"
            , placeholder = Nothing
            , show = False
            }
        , Input.button Attr.greenButton
            { onPress = Just <| Auth Auth.StartLogin
            , label = el [ centerX ] <| text "Login"
            }
        , el Attr.error <| text model.lastError
        ]


menuPanel : AppState -> Element Msg
menuPanel appState =
    let
        items =
            [ el [ pointer, onClick CreatePlan ] <| text "New plan" ]
                ++ (case appState.auth.sessionId of
                        Just sessionId ->
                            [ el [ pointer, onClick (RequestSavedPlans sessionId) ] <|
                                text "Saved plans"
                            , el [ pointer, onClick RequestLogout ] <| text "Logout"
                            ]

                        Nothing ->
                            [ el [ pointer, onClick RequestLogin ] <| text "Login"
                            , el [ pointer, onClick RequestRegistration ] <|
                                text "Register"
                            ]
                   )

        panel =
            column
                [ Background.color white
                , Border.widthEach { left = 1, right = 0, top = 0, bottom = 0 }
                , Border.color grey
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 1
                    , blur = 10
                    , color = lightCharcoal
                    }
                , Font.bold
                , Font.color darkCharcoal
                , Font.family [ Font.sansSerif ]
                , width <| fillPortion 1
                , height fill
                , paddingXY 20 20
                , spacingXY 0 20
                ]
                items

        overlay =
            el [ width <| fillPortion 4, height fill, onClick ToggleMenu ] none
    in
    if appState.isMenuOpen then
        row [ width fill, height fill ] [ overlay, panel ]

    else
        none


navBar : Element Msg
navBar =
    row
        [ width fill
        , paddingXY 60 10
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color blue
        ]
        [ el [ alignLeft ] <| text "VisExp"
        , Input.button (Attr.greyButton ++ [ padding 5, alignRight, width (px 80) ])
            { onPress = Just ToggleMenu
            , label = el [ centerX ] <| text "Menu"
            }
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
