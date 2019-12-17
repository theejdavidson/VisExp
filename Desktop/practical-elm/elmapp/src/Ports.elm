port module Ports exposing (dumpModel, saveSessionId)

port dumpModel : (() -> msg) -> Sub msg

port saveSessionId : Maybe String -> Cmd msg