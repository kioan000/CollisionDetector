module Collidable.Point exposing
    ( Point
    , distance
    , getX
    , getY
    , mapX
    , mapY
    , point
    , setX
    , setY
    )


type Point
    = Point
        { x : Float
        , y : Float
        }


point : Float -> Float -> Point
point x y =
    Point { x = x, y = y }


getX : Point -> Float
getX (Point { x }) =
    x


getY : Point -> Float
getY (Point { y }) =
    y


setX : Float -> Point -> Point
setX x (Point config) =
    Point { config | x = x }


setY : Float -> Point -> Point
setY y (Point config) =
    Point { config | y = y }


mapX : (Float -> Float) -> Point -> Point
mapX morph p =
    setX (morph (getX p)) p


mapY : (Float -> Float) -> Point -> Point
mapY morph p =
    setY (morph (getY p)) p


distance : Point -> Point -> Float
distance p1 p2 =
    let
        ( x1, y1 ) =
            ( getX p1, getY p1 )

        ( x2, y2 ) =
            ( getX p2, getY p2 )

        quadX =
            (x2 - x1) ^ 2

        quadY =
            (y2 - y1) ^ 2
    in
    sqrt (quadX + quadY)
