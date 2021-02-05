module Collidable.BoundingBox exposing
    ( BoundingBox
    , bottom
    , distance
    , fromTopLeft
    , intersects
    , left
    , moveTo
    , pickBottomLeft
    , pickBottomRight
    , pickCenter
    , pickHeight
    , pickTopLeft
    , pickTopRight
    , pickWidth
    , right
    , top
    , view
    )

import Collidable.Point as Point exposing (Point)
import Html exposing (Html, div)
import Html.Attributes exposing (style)


type BoundingBox
    = BoundingBox
        { topLeft : Point
        , topRight : Point
        , bottomLeft : Point
        , bottomRight : Point
        , center : Point
        , width : Float
        , height : Float
        }


fromTopLeft : Point -> Float -> Float -> BoundingBox
fromTopLeft topLeft height width =
    BoundingBox
        { topLeft = topLeft
        , topRight = Point.mapX ((+) width) topLeft
        , bottomLeft = Point.mapY ((+) height) topLeft
        , bottomRight = topLeft |> Point.mapX ((+) width) |> Point.mapY ((+) height)
        , center = topLeft |> Point.mapX ((+) (width / 2)) |> Point.mapY ((+) (height / 2))
        , width = width
        , height = height
        }


moveTo : Point -> BoundingBox -> BoundingBox
moveTo topLeft (BoundingBox config) =
    fromTopLeft topLeft config.height config.width


pickCenter : BoundingBox -> Point
pickCenter (BoundingBox { center }) =
    center


pickTopLeft : BoundingBox -> Point
pickTopLeft (BoundingBox { topLeft }) =
    topLeft


pickBottomLeft : BoundingBox -> Point
pickBottomLeft (BoundingBox { bottomLeft }) =
    bottomLeft


pickBottomRight : BoundingBox -> Point
pickBottomRight (BoundingBox { bottomRight }) =
    bottomRight


pickTopRight : BoundingBox -> Point
pickTopRight (BoundingBox { topRight }) =
    topRight


distance : BoundingBox -> BoundingBox -> Float
distance b1 b2 =
    Point.distance (pickCenter b1) (pickCenter b2)


pickHeight : BoundingBox -> Float
pickHeight (BoundingBox { height }) =
    height


pickWidth : BoundingBox -> Float
pickWidth (BoundingBox { width }) =
    width


{-| Draws bounding box for debugging purpose (fixed coordinates from 0,0 top left corner of browser view)
-}
view : List (Html.Attribute msg) -> BoundingBox -> Html msg
view attrs bb =
    div
        (List.append
            [ style "position" "fixed"
            , style "background-color" "none"
            , style "border" "1px solid red"
            , topStyle bb
            , leftStyle bb
            , heightStyle bb
            , widthStyle bb
            ]
            attrs
        )
        []


topStyle : BoundingBox -> Html.Attribute msg
topStyle =
    top >> String.fromFloat >> (\v -> v ++ "px") >> style "top"


leftStyle : BoundingBox -> Html.Attribute msg
leftStyle =
    left >> String.fromFloat >> (\v -> v ++ "px") >> style "left"


heightStyle : BoundingBox -> Html.Attribute msg
heightStyle =
    pickHeight >> (+) -2 >> String.fromFloat >> (\v -> v ++ "px") >> style "height"


widthStyle : BoundingBox -> Html.Attribute msg
widthStyle =
    pickWidth >> (+) -1 >> String.fromFloat >> (\v -> v ++ "px") >> style "width"


xProjectionIntersects : BoundingBox -> BoundingBox -> Bool
xProjectionIntersects b1 b2 =
    left b1 <= right b2 && right b1 >= left b2


yProjectionIntersects : BoundingBox -> BoundingBox -> Bool
yProjectionIntersects b1 b2 =
    top b1 <= bottom b2 && bottom b1 >= top b2


top : BoundingBox -> Float
top =
    pickTopLeft >> Point.getY


bottom : BoundingBox -> Float
bottom =
    pickBottomLeft >> Point.getY


right : BoundingBox -> Float
right =
    pickBottomRight >> Point.getX


left : BoundingBox -> Float
left =
    pickBottomLeft >> Point.getX


intersects : BoundingBox -> BoundingBox -> Bool
intersects b1 b2 =
    xProjectionIntersects b1 b2 && yProjectionIntersects b1 b2
