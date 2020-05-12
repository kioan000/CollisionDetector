module Utils.QuadTree.Boundary exposing (Boundary, containsPoint, create, intersects)

import Utils.QuadTree.Point as Point exposing (Point)


type Boundary
    = Boundary Config


type alias Config =
    { center : Point
    , halfDimension : Float
    }


create : Point -> Float -> Boundary
create center halfDimension =
    Boundary
        { center = center
        , halfDimension = halfDimension
        }


containsPoint : Point -> Boundary -> Bool
containsPoint point (Boundary { center, halfDimension }) =
    let
        inX : Bool
        inX =
            (&&) (Point.getX center - halfDimension <= Point.getX point) (Point.getX point <= Point.getX center + halfDimension)

        inY : Bool
        inY =
            (&&) (Point.getY center - halfDimension <= Point.getY point) (Point.getY point <= Point.getY center + halfDimension)
    in
    inX && inY


topLeft : Boundary -> Point
topLeft (Boundary { center, halfDimension }) =
    Point.create (Point.getX center - halfDimension) (Point.getY center - halfDimension)


topRight : Boundary -> Point
topRight (Boundary { center, halfDimension }) =
    Point.create (Point.getX center + halfDimension) (Point.getY center + halfDimension)


bottomLeft : Boundary -> Point
bottomLeft (Boundary { center, halfDimension }) =
    Point.create (Point.getX center - halfDimension) (Point.getY center - halfDimension)


bottomRight : Boundary -> Point
bottomRight (Boundary { center, halfDimension }) =
    Point.create (Point.getX center + halfDimension) (Point.getY center - halfDimension)


intersects : Boundary -> Boundary -> Bool
intersects boundary1 boundary2 =
    containsPoint (topLeft boundary1) boundary2
        |> (||) (containsPoint (topLeft boundary2) boundary1)
        |> (||) (containsPoint (topRight boundary1) boundary2)
        |> (||) (containsPoint (topRight boundary2) boundary1)
        |> (||) (containsPoint (bottomLeft boundary1) boundary2)
        |> (||) (containsPoint (bottomLeft boundary2) boundary1)
        |> (||) (containsPoint (bottomRight boundary1) boundary2)
        |> (||) (containsPoint (bottomRight boundary2) boundary1)
