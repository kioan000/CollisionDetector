module Utils.QuadTree.Point exposing (Point, create, getX, getY)


type Point
    = Point Config


type alias Config =
    { x : Float
    , y : Float
    }


create : Float -> Float -> Point
create x y =
    Point { x = x, y = y }


getX : Point -> Float
getX (Point { x }) =
    x


getY : Point -> Float
getY (Point { y }) =
    y
