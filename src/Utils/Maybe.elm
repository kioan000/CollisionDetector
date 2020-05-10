module Utils.Maybe exposing (..)


isJust : Maybe a -> Bool
isJust theMaybe =
    case theMaybe of
        Just _ ->
            True

        Nothing ->
            False
