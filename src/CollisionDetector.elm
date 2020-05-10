module CollisionDetector exposing
    ( CollisionDetector
    , addCollidable
    , create
    , cycle
    , cycleWithCmds
    , updateHeight
    , updatePosition
    , updateWidth
    )

import Collidable exposing (Collidable)
import Dict exposing (Dict)
import Utils.Maybe as UtilsMaybe
import Utils.Update as UtilsUpdate


type CollisionDetector scope msg
    = CollisionDetector (Config scope msg)


type alias Config scope msg =
    { collidables : Dict String (Collidable scope msg)
    }


cycle : CollisionDetector scope msg -> { model | collisionDetectorScope : scope } -> ( { model | collisionDetectorScope : scope }, Cmd msg )
cycle collisionDetector model =
    cycleWithCmds collisionDetector ( model, Cmd.none )


cycleWithCmds : CollisionDetector scope msg -> ( { model | collisionDetectorScope : scope }, Cmd msg ) -> ( { model | collisionDetectorScope : scope }, Cmd msg )
cycleWithCmds config ( { collisionDetectorScope } as model, cmd ) =
    config
        |> pickConfig
        |> .collidables
        |> iterateCollisions ( collisionDetectorScope, cmd )
        |> reWrapScope model


reWrapScope : { model | collisionDetectorScope : scope } -> ( scope, Cmd msg ) -> ( { model | collisionDetectorScope : scope }, Cmd msg )
reWrapScope model ( scope, cmds ) =
    ( { model | collisionDetectorScope = scope }, cmds )


create : CollisionDetector scope msg
create =
    CollisionDetector
        { collidables = Dict.empty
        }


addCollidable : Collidable scope msg -> CollisionDetector scope msg -> CollisionDetector scope msg
addCollidable collidable (CollisionDetector config) =
    CollisionDetector { config | collidables = Dict.insert (Collidable.pickId collidable) collidable config.collidables }


updatePosition : String -> Float -> Float -> CollisionDetector scope msg -> CollisionDetector scope msg
updatePosition collidableId x y (CollisionDetector config) =
    CollisionDetector { config | collidables = Dict.update collidableId (Maybe.map (Collidable.updatePosition x y)) config.collidables }


updateHeight : String -> Float -> CollisionDetector scope msg -> CollisionDetector scope msg
updateHeight collidableId height (CollisionDetector config) =
    CollisionDetector { config | collidables = Dict.update collidableId (Maybe.map (Collidable.updateHeight height)) config.collidables }


updateWidth : String -> Float -> CollisionDetector scope msg -> CollisionDetector scope msg
updateWidth collidableId width (CollisionDetector config) =
    CollisionDetector { config | collidables = Dict.update collidableId (Maybe.map (Collidable.updateWidth width)) config.collidables }


pickConfig : CollisionDetector scope msg -> Config scope msg
pickConfig (CollisionDetector config) =
    config


iterateCollisions : ( scope, Cmd msg ) -> Dict String (Collidable scope msg) -> ( scope, Cmd msg )
iterateCollisions ( scope, cmd ) collidables =
    case Dict.values collidables of
        x1 :: xs ->
            List.foldl
                (collisionMapper x1)
                ( scope, cmd )
                xs
                |> verifyCollision x1 xs

        _ ->
            ( scope, cmd )


verifyCollision : Collidable scope msg -> List (Collidable scope msg) -> ( scope, Cmd msg ) -> ( scope, Cmd msg )
verifyCollision collidable remaining ( scope, cmd ) =
    case remaining of
        [] ->
            ( scope, cmd )

        _ :: [] ->
            ( scope, cmd )

        x :: xs ->
            List.foldl
                (collisionMapper collidable)
                ( scope, cmd )
                remaining
                |> verifyCollision x xs


collisionMapper : Collidable scope msg -> Collidable scope msg -> ( scope, Cmd msg ) -> ( scope, Cmd msg )
collisionMapper collidable1 collidable2 ( scope, cmd ) =
    if UtilsMaybe.isJust (Collidable.pickCollisionHandler collidable1) || UtilsMaybe.isJust (Collidable.pickCollisionHandler collidable2) then
        if Collidable.areCollided collidable1 collidable2 then
            ( scope, cmd )
                |> collisionUpdater collidable1 collidable2
                |> collisionUpdater collidable2 collidable1

        else
            ( scope, cmd )

    else
        ( scope, cmd )


collisionUpdater : Collidable scope msg -> Collidable scope msg -> ( scope, Cmd msg ) -> ( scope, Cmd msg )
collisionUpdater collidable1 collidable2 ( scope, cmd ) =
    collidable1
        |> Collidable.pickCollisionHandler
        |> Maybe.map
            (\collisionHanlder ->
                scope
                    |> collisionHanlder collidable1 collidable2
                    |> UtilsUpdate.addCmd cmd
            )
        |> Maybe.withDefault ( scope, cmd )
