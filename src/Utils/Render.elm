module Utils.Render exposing (maybeMap, when)

import Html exposing (Html, text)


dummy : Html msg
dummy =
    text ""


when : Bool -> Html msg -> Html msg
when condition el =
    if condition then
        el

    else
        dummy


maybeMap : (a -> Html msg) -> Maybe a -> Html msg
maybeMap renderFun =
    Maybe.map renderFun
        >> Maybe.withDefault dummy
