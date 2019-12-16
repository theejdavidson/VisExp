module Attr exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

greenButton : List (Attribute msg)
greenButton =
    [ Background.color green
    , Border.color darkGreen
    , Border.rounded 3
    , Border.widthEach { bottom = 3, top = 0, right = 0, left = 0 }
    , Font.bold
    , Font.color white
    , paddingXY 20 6
    ]

greyButton : List (Attribute msg)
greyButton =
    [ Background.color lightGrey
    , Border.color grey
    , Border.rounded 3
    , Border.widthEach { bottom = 1, right = 1, top = 0, left = 0 }
    , Font.bold]

blue : Color
blue =
    rgb255 52 101 164


lightBlue : Color
lightBlue =
    rgb255 139 178 248


lightYellow : Color
lightYellow =
    rgb255 255 255 96


white : Color
white =
    rgb255 255 255 255


lightCharcoal : Color
lightCharcoal =
    rgb255 136 138 133


lightGrey : Color
lightGrey =
    rgb255 226 226 226


grey : Color
grey =
    rgb255 145 145 145


green : Color
green =
    rgb255 0 97 43


darkGreen : Color
darkGreen =
    rgb255 0 141 0