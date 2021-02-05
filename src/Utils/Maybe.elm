module Utils.Maybe exposing (isJust)


isJust : Maybe a -> Bool
isJust theMaybe =
    case theMaybe of
        Just _ ->
            True

        Nothing ->
            False
