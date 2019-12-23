module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html as HTML exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)



---- MODEL ----


type alias Model =
    { jsonTextA : String }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { jsonTextA = "" }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- UPDATE ----


type Msg
    = NoOp
    | JsonTextA String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "JsonDiff"
    , body =
        [ layout [] <|
            column [ width fill, spacingXY 0 20 ]
                [ jsonA model
                ]
        ]
    }


jsonA : model -> Element Msg
jsonA model =
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
            { onChange = JsonTextA
            , text = "jsonTextA"
            , placeholder = Nothing
            , label =
                Input.labelAbove [] <|
                    Element.text "Paste the JSON text below:"
            , spellcheck = False
            }
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Color attributes


lightCharcoal : Color
lightCharcoal =
    rgb255 136 138 133
