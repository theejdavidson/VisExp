module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz
import Json.Decode
import PlanParsers.Json exposing (..)
import Test exposing (..)
import Utils exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


expectSortedVersions : SavedPlan -> Expectation
expectSortedVersions plan =
    let
        versions =
            List.map .version plan.versions
    in
    if List.sort versions == versions then
        Expect.pass

    else
        Expect.fail "Versions aren't sorted"


suite : Test
suite =
    describe "Saved plan JSON decoder"
        [ test "Decoder converts valid strings" <|
            \_ ->
                let
                    json =
                        """
                        [
                            { "id": "1"
                            , "name": "Project query plan"
                            , "versions": [ { "version": 1
                                            , "createdAt": "2019-12-20"
                                            , "planText": "{}"
                                            }
                                        ]
                            }
                        ]
                        """

                    result =
                        Json.Decode.decodeString decodeSavedPlans json
                in
                Expect.equal result
                    (Ok
                        [ { id = "1"
                          , name = "Project query plan"
                          , versions =
                                [ { version = 1
                                  , createdAt = "2019-12-20"
                                  , planText = "{}"
                                  }
                                ]
                          }
                        ]
                    )
        , test "Decoder fails on missing fields" <|
            \_ ->
                let
                    json =
                        """
                        [{}]
                        """

                    result =
                        Json.Decode.decodeString decodeSavedPlans json
                in
                Expect.err result
        , test "Version order" <|
            \_ ->
                expectSortedVersions
                    { id = "1"
                    , name = "Plan"
                    , versions =
                        [ { version = 1, createdAt = "2019-12-01", planText = "{}" }
                        , { version = 3, createdAt = "2019-12-02", planText = "{}" }
                        , { version = 5, createdAt = "2019-12-03", planText = "{}" }
                        ]
                    }
        ]


suite2 : Test
suite2 =
    describe "Node calculations"
        [ fuzz nodeFuzzer "Node duration should not exceed total time" <|
            \node ->
                calcDuration node
                    |> Expect.atMost (calcNodeTime node)
        ]


nodeFuzzer : Fuzz.Fuzzer CommonFields
nodeFuzzer =
    let
        timeRange =
            Fuzz.floatRange 0.001 1000.0

        loopRange =
            Fuzz.intRange 1 128
    in
    Fuzz.map4 makeNode
        timeRange
        loopRange
        (Fuzz.list timeRange)
        (Fuzz.list loopRange)


makeNode : Float -> Int -> List Float -> List Int -> CommonFields
makeNode actualTotalTime actualLoops subTimes subLoops =
    let
        makeSubNode ( time, loops ) =
            PGeneric
                { actualLoops = loops
                , actualTotalTime = time
                , nodeType = "Generic"
                , plans = Plans []
                , relationName = ""
                , schema = ""
                , startupCost = 0
                , totalCost = 0
                }

        plans =
            subLoops
                |> List.map2 Tuple.pair subTimes
                |> List.map makeSubNode
    in
    { actualLoops = actualLoops
    , actualTotalTime = actualTotalTime
    , nodeType = "Generic"
    , plans = Plans plans
    , relationName = ""
    , schema = ""
    , startupCost = 0
    , totalCost = 0
    }
