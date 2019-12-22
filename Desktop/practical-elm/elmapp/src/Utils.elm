module Utils exposing (..)

import Http
import PlanParsers.Json exposing (..)

httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadBody message ->
            "Unable to handle response: " ++ message

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.NetworkError ->
            "Network error"

        Http.Timeout ->
            "Request timeout"

unwrap : Plan -> CommonFields
unwrap plan =
    case plan of
        PCte p ->
            p.common

        PGeneric p ->
            p

        PResult p ->
            p.common

        PSeqScan p ->
            p.common

        PSort p ->
            p.common

calcNodeTime : CommonFields -> Float
calcNodeTime node =
    node.actualTotalTime * toFloat node.actualLoops

calcDuration : CommonFields -> Float
calcDuration node =
    let
        (Plans planList) =
            node.plans
    in
    calcNodeTime node
        - (List.sum <| List.map (unwrap >> calcDuration) planList)