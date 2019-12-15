module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img)
import Html.Attributes exposing (src)
import Element exposing (..)
import Element.Border as Border



---- MODEL ----


type Page
    = InputPage

type Msg
    = NoOp


type alias Model =
    { currPage : Page
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = InputPage
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- UPDATE ----



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view _ =
    { title = "VisExp"
    , body = 
        [ layout [] <|
            column [ width fill, spacingXY 0 20 ]
                [ navBar
                , el [ centerX ] <| text "VisExp"
                ]
        ]
    }

blue : Color
blue = rgb255 52 101 164

navBar : Element Msg
navBar =
    row
        [ width fill
        , paddingXY 60 10
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color blue
        ]
        [ el [ alignLeft ] <| text "VisExp"
        , el [ alignRight ] <| text "Menu"
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
