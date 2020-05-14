module Utils.QuadTree.Boundary exposing (Boundary, bottomLeftSplit, bottomRightSplit, containsPoint, create, intersects, topLeftSplit, topRightSplit)

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


topLeftCorner : Boundary -> Point
topLeftCorner (Boundary { center, halfDimension }) =
    Point.create (Point.getX center - halfDimension) (Point.getY center - halfDimension)


topRightCorner : Boundary -> Point
topRightCorner (Boundary { center, halfDimension }) =
    Point.create (Point.getX center + halfDimension) (Point.getY center + halfDimension)


bottomLeftCorner : Boundary -> Point
bottomLeftCorner (Boundary { center, halfDimension }) =
    Point.create (Point.getX center - halfDimension) (Point.getY center - halfDimension)


bottomRightCorner : Boundary -> Point
bottomRightCorner (Boundary { center, halfDimension }) =
    Point.create (Point.getX center + halfDimension) (Point.getY center - halfDimension)


topLeftSplit : Boundary -> Boundary
topLeftSplit (Boundary { center, halfDimension }) =
    Boundary
        { center = Point.create (Point.getX center - halfDimension / 2) (Point.getY center + halfDimension / 2)
        , halfDimension = halfDimension / 2
        }


topRightSplit : Boundary -> Boundary
topRightSplit (Boundary { center, halfDimension }) =
    Boundary
        { center = Point.create (Point.getX center + halfDimension / 2) (Point.getY center + halfDimension / 2)
        , halfDimension = halfDimension / 2
        }


bottomLeftSplit : Boundary -> Boundary
bottomLeftSplit (Boundary { center, halfDimension }) =
    Boundary
        { center = Point.create (Point.getX center - halfDimension / 2) (Point.getY center - halfDimension / 2)
        , halfDimension = halfDimension / 2
        }


bottomRightSplit : Boundary -> Boundary
bottomRightSplit (Boundary { center, halfDimension }) =
    Boundary
        { center = Point.create (Point.getX center + halfDimension / 2) (Point.getY center - halfDimension / 2)
        , halfDimension = halfDimension / 2
        }


intersects : Boundary -> Boundary -> Bool
intersects boundary1 boundary2 =
    containsPoint (topLeftCorner boundary1) boundary2
        |> (||) (containsPoint (topLeftCorner boundary2) boundary1)
        |> (||) (containsPoint (topRightCorner boundary1) boundary2)
        |> (||) (containsPoint (topRightCorner boundary2) boundary1)
        |> (||) (containsPoint (bottomLeftCorner boundary1) boundary2)
        |> (||) (containsPoint (bottomLeftCorner boundary2) boundary1)
        |> (||) (containsPoint (bottomRightCorner boundary1) boundary2)
        |> (||) (containsPoint (bottomRightCorner boundary2) boundary1)
