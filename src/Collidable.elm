module Collidable exposing
    ( Collidable
    , CollisionEvent
    , areCollided
    , collidable
    , pickBoundingBox
    , pickCollisionEvent
    , pickId
    , updateBoundingBox
    )

import Collidable.BoundingBox as BoundingBox exposing (BoundingBox)
import Collidable.Point as Point exposing (Point)


type Collidable msg
    = Collidable (Config msg)


type alias Config msg =
    { id : String
    , boundingBox : BoundingBox
    , collisionEvent : Maybe (CollisionEvent msg)
    }


{-| Type alias for collision handler function.
-}
type alias CollisionEvent msg =
    Collidable msg -> Collidable msg -> msg


collidable : String -> Point -> Float -> Float -> Maybe (CollisionEvent msg) -> Collidable msg
collidable id topLeft height width event =
    Collidable
        { id = id
        , boundingBox = BoundingBox.fromTopLeft topLeft height width
        , collisionEvent = event
        }


updateBoundingBox : Point -> Float -> Float -> Collidable msg -> Collidable msg
updateBoundingBox topLeft height width (Collidable config) =
    Collidable
        { config
            | boundingBox = BoundingBox.fromTopLeft topLeft height width
        }


areCollided : Collidable msg -> Collidable msg -> Bool
areCollided c1 c2 =
    BoundingBox.intersects (pickBoundingBox c1) (pickBoundingBox c2)


pickId : Collidable msg -> String
pickId (Collidable config) =
    config.id


pickCollisionEvent : Collidable msg -> Maybe (CollisionEvent msg)
pickCollisionEvent (Collidable config) =
    config.collisionEvent


pickBoundingBox : Collidable msg -> BoundingBox
pickBoundingBox (Collidable { boundingBox }) =
    boundingBox
