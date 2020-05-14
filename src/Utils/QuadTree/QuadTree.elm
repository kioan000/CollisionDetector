module Utils.QuadTree.QuadTree exposing (QuadTree, create, insert)

import Utils.QuadTree.Boundary as Boundary exposing (Boundary)
import Utils.QuadTree.Point as Point exposing (Point)


type QuadTree
    = Root RootConfig
    | Leaf LeafConfig


type alias RootConfig =
    { boundary : Boundary
    , topLeft : QuadTree
    , topRight : QuadTree
    , bottomLeft : QuadTree
    , bottomRight : QuadTree
    }


leafCapacity : Int
leafCapacity =
    8


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


containsPoint : Point -> QuadTree -> Bool
containsPoint point quadTree =
    case quadTree of
        Root rootConfig ->
            Boundary.containsPoint point rootConfig.boundary

        Leaf leafConfig ->
            Boundary.containsPoint point leafConfig.boundary


split : QuadTree -> Result QuadTree QuadTree
split quadTree =
    case quadTree of
        Root rootConfig ->
            Result.map4
                (\tL tR bL bR ->
                    Root
                        { rootConfig
                            | topLeft = tL
                            , topRight = tR
                            , bottomLeft = bL
                            , bottomRight = bR
                        }
                )
                (split rootConfig.topLeft)
                (split rootConfig.topRight)
                (split rootConfig.bottomLeft)
                (split rootConfig.bottomRight)
                |> Result.mapError (always quadTree)

        Leaf ({ points, boundary } as leafConfig) ->
            if List.length points <= leafCapacity then
                Leaf leafConfig |> Result.Ok

            else
                let
                    newTree =
                        Root
                            { boundary = boundary
                            , topLeft = create (Boundary.topLeftSplit boundary)
                            , topRight = create (Boundary.topRightSplit boundary)
                            , bottomLeft = create (Boundary.bottomLeftSplit boundary)
                            , bottomRight = create (Boundary.bottomRightSplit boundary)
                            }
                            |> Result.Ok
                in
                List.foldl
                    (\p ->
                        Result.andThen (insert p)
                            >> Result.mapError (always quadTree)
                    )
                    newTree
                    points


insert : Point -> QuadTree -> Result QuadTree QuadTree
insert point quadTree =
    case ( containsPoint point quadTree, quadTree ) of
        ( True, Root treeConfig ) ->
            if containsPoint point treeConfig.topLeft then
                treeConfig
                    |> .topLeft
                    |> insert point
                    |> Result.map (\uTL -> Root { treeConfig | topLeft = uTL })
                    |> Result.mapError (always quadTree)

            else if containsPoint point treeConfig.topRight then
                treeConfig
                    |> .topRight
                    |> insert point
                    |> Result.map (\uTR -> Root { treeConfig | topRight = uTR })
                    |> Result.mapError (always quadTree)

            else if containsPoint point treeConfig.topRight then
                treeConfig
                    |> .bottomLeft
                    |> insert point
                    |> Result.map (\uBL -> Root { treeConfig | bottomLeft = uBL })
                    |> Result.mapError (always quadTree)

            else if containsPoint point treeConfig.topRight then
                treeConfig
                    |> .bottomRight
                    |> insert point
                    |> Result.map (\uBR -> Root { treeConfig | bottomRight = uBR })
                    |> Result.mapError (always quadTree)

            else
                Result.Err quadTree

        ( True, Leaf leafConfig ) ->
            if List.length leafConfig.points < leafCapacity then
                { leafConfig
                    | points = point :: leafConfig.points
                }
                    |> Leaf
                    |> Result.Ok

            else
                quadTree
                    |> split
                    |> Result.andThen (insert point)
                    |> Result.mapError (always quadTree)

        ( False, _ ) ->
            Result.Err quadTree
