module Collidable exposing
    ( Collidable
    , CollisionEvent
    , areCollided
    , collidable
    , pickBoundingBox
    , pickCollisionEvent
    , pickId
    , track
    , updateBoundingBox
    , withCollisionEvent
    )

import Collidable.BoundingBox as BoundingBox exposing (BoundingBox)
import Collidable.Point as Point exposing (Point)


type Collidable msg
    = Tracked (Config msg)
    | UnTracked (UnTrackedConfig msg)


collidable : String -> Collidable msg
collidable id =
    UnTracked
        { id = id
        , collisionEvent = Nothing
        }


withCollisionEvent : CollisionEvent msg -> Collidable msg -> Collidable msg
withCollisionEvent evt el =
    case el of
        Tracked config ->
            Tracked { config | collisionEvent = Just evt }

        UnTracked unTrackedConfig ->
            UnTracked { unTrackedConfig | collisionEvent = Just evt }


type alias UnTrackedConfig msg =
    { id : String
    , collisionEvent : Maybe (CollisionEvent msg)
    }


type alias Config msg =
    { id : String
    , boundingBox : BoundingBox
    , collisionEvent : Maybe (CollisionEvent msg)
    }


{-| Type alias for collision handler function.
-}
type alias CollisionEvent msg =
    Collidable msg -> Collidable msg -> msg


track : Point -> Float -> Float -> Collidable msg -> Collidable msg
track topLeft height width coll =
    case coll of
        Tracked trackedConfig ->
            Tracked
                { id = trackedConfig.id
                , boundingBox = BoundingBox.fromTopLeft topLeft height width
                , collisionEvent = trackedConfig.collisionEvent
                }

        UnTracked unTrackedConfig ->
            Tracked
                { id = unTrackedConfig.id
                , boundingBox = BoundingBox.fromTopLeft topLeft height width
                , collisionEvent = unTrackedConfig.collisionEvent
                }


updateBoundingBox : Point -> Float -> Float -> Collidable msg -> Collidable msg
updateBoundingBox topLeft height width coll =
    case coll of
        Tracked config ->
            Tracked
                { config
                    | boundingBox = BoundingBox.fromTopLeft topLeft height width
                }

        untracked ->
            untracked


areCollided : Collidable msg -> Collidable msg -> Bool
areCollided c1 c2 =
    Maybe.map2
        BoundingBox.intersects
        (pickBoundingBox c1)
        (pickBoundingBox c2)
        |> Maybe.withDefault False


pickId : Collidable msg -> String
pickId coll =
    case coll of
        Tracked config ->
            config.id

        UnTracked unTrackedConfig ->
            unTrackedConfig.id


pickCollisionEvent : Collidable msg -> Maybe (CollisionEvent msg)
pickCollisionEvent coll =
    case coll of
        Tracked config ->
            config.collisionEvent

        UnTracked unTrackedConfig ->
            unTrackedConfig.collisionEvent


pickBoundingBox : Collidable msg -> Maybe BoundingBox
pickBoundingBox coll =
    case coll of
        Tracked config ->
            Just config.boundingBox

        UnTracked unTrackedConfig ->
            Nothing
