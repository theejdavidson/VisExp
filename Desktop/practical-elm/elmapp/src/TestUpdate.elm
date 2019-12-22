module TestUpdate exposing (fuzz, fuzzBasic)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import Test.Runner


fuzz :
    (msg -> model -> ( model, cmd ))
    -> Fuzzer msg
    -> model
    -> String
    -> (model -> Expectation)
    -> Test
fuzz update =
    fuzzBasic (\msg model -> Tuple.first (update msg model))


fuzzBasic :
    (msg -> model -> model)
    -> Fuzzer msg
    -> model
    -> String
    -> (model -> Expectation)
    -> Test
fuzzBasic update msgFuzzer model description verify =
    Test.fuzz (Fuzz.list msgFuzzer) description <|
        test update model verify


test :
    (msg -> model -> model)
    -> model
    -> (model -> Expectation)
    -> List msg
    -> Expectation
test update model verify messages =
    case Test.Runner.getFailureReason (verify model) of
        Nothing ->
            testUpdateHelp Expect.pass update model verify messages

        Just { given, description, reason } ->
            [ "Initial model failed before any messages were applied."
            , ""
            , "Initial model was:"
            , ""
            , "  " ++ Debug.toString model
            , ""
            , "Failure was:"
            , ""
            , description
            ]
                |> String.join "\n"
                |> Expect.fail


testUpdateHelp :
    Expectation
    -> (msg -> model -> model)
    -> model
    -> (model -> Expectation)
    -> List msg
    -> Expectation
testUpdateHelp expectation update model verify messages =
    case messages of
        [] ->
            expectation

        msg :: rest ->
            let
                newModel =
                    update msg model
            in
            case Test.Runner.getFailureReason (verify newModel) of
                Nothing ->
                    testUpdateHelp expectation update newModel verify rest

                Just { given, description, reason } ->
                    [ "Model which passed:"
                    , ""
                    , "  " ++ Debug.toString model
                    , ""
                    , "Message applied to that model:"
                    , ""
                    , "  " ++ Debug.toString msg
                    , ""
                    , "Resulting model, which failed:"
                    , ""
                    , "  " ++ Debug.toString newModel
                    , ""
                    , "Failure:"
                    , ""
                    , description
                    ]
                        |> String.join "\n"
                        |> Expect.fail
