module Types exposing (SessionId, asString, makeSessionId)


type SessionId
    = SessionId String


makeSessionId : String -> Maybe SessionId
makeSessionId s =
    if String.isEmpty s then
        Nothing

    else
        Just (SessionId s)


asString : SessionId -> String
asString (SessionId id) =
    id