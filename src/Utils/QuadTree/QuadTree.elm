module Utils.QuadTree.QuadTree exposing (QuadTree, create)

import Utils.QuadTree.Boundary as Boundary exposing (Boundary)
import Utils.QuadTree.Point as Point exposing (Point)


type QuadTree
    = Tree TreeConfig
    | Leaf LeafConfig


type alias TreeConfig =
    { boundary : Boundary
    , topLeft : QuadTree
    , topRight : QuadTree
    , bottomLeft : QuadTree
    , bottomRight : QuadTree
    }

leafCapacity: Int
leafCapacity = 8


type alias LeafConfig =
    { boundary : Boundary
    , points : List Point
    }


create : Boundary -> QuadTree
create boundary =
    Leaf
        { boundary = boundary
        , points = []
        }


insert : Point -> QuadTree -> QuadTree
insert point quadTree =

    case quadTree of
        Tree {boundary} ->
            if ( not <| Boundary.containsPoint boundary) then quadTree else

        Leaf {boundary} ->
            if ( not <| Boundary.containsPoint boundary) then quadTree else
