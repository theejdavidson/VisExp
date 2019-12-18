module Main exposing (..)

import Attr exposing (..)
import Auth exposing (..)
import Browser
import Browser.Events exposing (onKeyPress)
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
import PlanParsers.Json exposing (..)
import PlanTree exposing (..)
import Ports exposing (dumpModel, saveSessionId)
import Time
import Utils exposing (..)
import Pages.Registration exposing (Model)
import Types exposing (..)



---- MODEL ----


type Page
    = DisplayPage Display.Model
    | InputPage
    | LoginPage
    | RegistrationPage Registration.Model
    | SavedPlansPage SavedPlans.Model


type Msg
    = ChangePlanText String
    | MouseEnteredPlanNode Plan
    | MouseLeftPlanNode Plan
    | SubmitPlan
    | ToggleMenu
    | CreatePlan
    | FinishSavedPlans (Result Http.Error (List SavedPlan))
    | RequestLogin
    | RequestLogout
    | RequestSavedPlans
    | ShowPlan String
    | DumpModel ()
    | Auth Auth.Msg
    | Register Registration.Msg


type alias Model =
    { appState : AppState
    , currPage : Page
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { auth = Auth.init flags.sessionId
      , currPage = InputPage
      , currPlanText = ""
      , isMenuOpen = False
      , lastError = ""
      , savedPlans = []
      , selectedNode = Nothing
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dumpModel DumpModel
        , Time.every (100 * 1000) SendHeartbeat
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
    case ( s, model.sessionId ) of
        ( "s", Just id ) ->
            RequestSavedPlans

        ( "n", _ ) ->
            CreatePlan

        _ ->
            NoOp



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Auth authMsg ->
            let
                ( authModel, authCmd ) =
                    Auth.update serverUrl authMsg model.auth

                currPage =
                    case authMsg of
                        Auth.FinishLogin (Ok _) ->
                            InputPage

                        _ ->
                            model.currPage
            in
            ( { model | auth = authModel, currPage = currPage }
            , Cmd.map Auth authCmd
            )

        RequestRegistration ->
            ( { model | currPage = RegistrationPage Registration.init }, Cmd.none )

        Register regMsg ->
            let
                (newAppState, regModel, regCmd ) =
                    Registration.update regMsg model.appState pageModel

                newCurrPage =
                    case regMsg of
                        Registration.FinishRegistration (Ok _) ->
                            InputPage

                        _ ->
                            RegistrationPage regModel
            in
            ( { appState = newAppState, currPage = newCurrPage }
            , Cmd.map Register regCmd
            )
            

        ChangePlanText s ->
            ( { model | currPlanText = s }, Cmd.none )

        CreatePlan ->
            ( { model | currPage = InputPage, currPlanText = "" }, Cmd.none )

        MouseEnteredPlanNode plan ->
            ( { model | selectedNode = Just plan }, Cmd.none )

        MouseLeftPlanNode commonFields ->
            ( { model | selectedNode = Nothing }, Cmd.none )

        ( RequestLogin, _ ) ->
            ( { model | currPage = LoginPage }
            , Cmd.none
            )

        RequestSavedPlans ->
            ( { model | currPage = SavedPlansPage }
            , getSavedPlans model.sessionId
            )

        ( SubmitPlan, InputPage ) ->
            ( { model | currPage = DisplayPage Display.init }, Cmd.none )

        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

        FinishSavedPlans (Ok savedPlans) ->
            ( { model | savedPlans = savedPlans }, Cmd.none )

        FinishSavedPlans (Err error) ->
            ( { model | lastError = httpErrorString error }, Cmd.none )

        ShowPlan planText ->
            ( { model | currPlanText = planText, currPage = DisplayPage }
            , Cmd.none
            )

        DumpModel () ->
            ( Debug.log "model" model, Cmd.none )

        (_, _ ) ->
            ( model, Cmd.none )


getSavedPlans : String -> Maybe String -> Cmd Msg
getSavedPlans serverUrl sessionId =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "SessionId" <| Maybe.withDefault "" sessionId ]
        , url = serverUrl ++ "plans"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson FinishSavedPlans decodeSavedPlans
        }


savedPlansPage : Model -> Element Msg
savedPlansPage model =
    let
        annotateVersion name planVersion =
            { version = planVersion.version
            , planText = planVersion.planText
            , createdAt = planVersion.createdAt
            , name = name
            }

        annotateVersions savedPlan =
            List.map (annotateVersion savedPlan.name) savedPlan.versions

        tableAttrs =
            [ width (px 800)
            , paddingEach { top = 10, bottom = 50, left = 10, right = 10 }
            , spacingXY 10 10
            , centerX
            ]

        headerAttrs =
            [ Font.bold
            , Background.color lightGrey
            , Border.color darkCharcoal
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , centerX
            ]
    in
    table tableAttrs
        { data = List.concatMap annotateVersions model.savedPlans
        , columns =
            [ { header = el headerAttrs <| text "Plan name"
              , width = fill
              , view =
                    \plan ->
                        el
                            [ Font.underline
                            , mouseOver [ Font.color lightCharcoal ]
                            , onClick <| ShowPlan plan.planText
                            ]
                        <|
                            text plan.name
              }
            , { header = el headerAttrs <| text "Creation time"
              , width = fill
              , view = .createdAt >> text
              }
            , { header = el headerAttrs <| text "Version"
              , width = fill
              , view = .version >> String.fromInt >> text
              }
            ]
        }



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
                    inputPage model

                LoginPage ->
                    loginPage model

                SavedPlansPage pageModel ->
                    SavedPlans.page pageModel
                        |> Element.map SavedPlans

                RegistrationPage pageModel ->
                    Registration.page pageModel
                        |> Element.map Register
    in
    { title = "VisExp"
    , body =
        [ layout [ inFront <| menuPanel model ] <|
            column [ width fill, spacingXY 0 20 ]
                [ navBar
                , content
                ]
        ]
    }


inputPage : Model -> Element Msg
inputPage model =
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
            , text = model.currPlanText
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

loginPage : Model -> Element Msg
loginPage model =
    column [ paddingXY 0 20, spacingXY 0 10, width (px 300), centerX ]
        [ Input.username Attr.input
            { onChange = ChangeUserName
            , text = model.userName
            , label = Input.labelAbove [] <| text "User name:"
            , placeholder = Nothing
            }
        , Input.currentPassword Attr.input
            { onChange = ChangePassword
            , text = model.password
            , label = Input.labelAbove [] <| text "Password:"
            , placeholder = Nothing
            , show = False
            }
        , Input.button Attr.greenButton
            { onPress = Just StartLogin
            , label = el [ centerX ] <| text "Login"
            }
        , el Attr.error <| text model.lastError
        ]


displayPage : Model -> Element Msg
displayPage model =
    let
        planTreeConfig =
            { onMouseEnteredNode = MouseEnteredPlanNode
            , onMouseLeftNode = MouseLeftPlanNode
            }
    in
    case Json.Decode.decodeString decodePlanJson model.currPlanText of
        Ok planJson ->
            PlanTree.render planTreeConfig planJson model.selectedNode

        Err err ->
            el [] <| text <| Json.Decode.errorToString err


menuPanel : Model -> Element Msg
menuPanel model =
    let
        items =
            [ el [ pointer, onClick CreatePlan ] <| text "New plan" ]
                ++ (case model.sessionId of
                        Just _ ->
                            [ el [ pointer, onClick RequestSavedPlans ] <|
                                text "Saved plans"
                            , el [ pointer, onClick RequestLogout ] <| text "Logout"
                            ]

                        Nothing ->
                            [ el [ pointer, onClick RequestLogin ] <| text "Login" ]
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
    if model.isMenuOpen then
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
