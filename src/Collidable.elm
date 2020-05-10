module Collidable exposing
    ( Collidable
    , CollisionHandler
    , areCollided
    , create
    , onCollision
    , pickCollisionHandler
    , pickId
    , updateHeight
    , updatePosition
    , updateWidth
    )


type Collidable scope msg
    = Collidable (Config scope msg)


type alias Config scope msg =
    { id : String
    , boundingBox : BoundingBox
    , collisionBoxType : CollisionBoxType
    , onCollision : Maybe (CollisionHandler scope msg)
    , collidesWith : CollidesWithOption
    , height : Float
    , width : Float
    , unit : CoordinatesUnit
    }


{-| Type alias for collision handler function.
-}
type alias CollisionHandler scope msg =
    Collidable scope msg -> Collidable scope msg -> scope -> ( scope, Cmd msg )


type CoordinatesUnit
    = Pixel


type CollidesWithOption
    = All


type alias Point =
    { x : Float
    , y : Float
    }


type alias BoundingBox =
    { topLeft : Point
    , topRight : Point
    , bottomLeft : Point
    , bottomRight : Point
    }


type CollisionBoxType
    = Squared


create : String -> Float -> Float -> Float -> Float -> Collidable scope msg
create id x y height width =
    Collidable
        { id = id
        , boundingBox = createBoundingBox x y height width
        , collisionBoxType = Squared
        , onCollision = Nothing
        , collidesWith = All
        , height = height
        , width = width
        , unit = Pixel
        }


onCollision : CollisionHandler scope msg -> Collidable scope msg -> Collidable scope msg
onCollision collisionEvent (Collidable config) =
    Collidable
        { config
            | onCollision = Just collisionEvent
        }


updatePosition : Float -> Float -> Collidable scope msg -> Collidable scope msg
updatePosition x y (Collidable config) =
    Collidable
        { config
            | boundingBox = createBoundingBox x y config.height config.width
        }


updateHeight : Float -> Collidable scope msg -> Collidable scope msg
updateHeight height (Collidable config) =
    Collidable
        { config
            | boundingBox = createBoundingBox config.boundingBox.topLeft.x config.boundingBox.topLeft.y height config.width
            , height = height
        }


updateWidth : Float -> Collidable scope msg -> Collidable scope msg
updateWidth width (Collidable config) =
    Collidable
        { config
            | boundingBox = createBoundingBox config.boundingBox.topLeft.x config.boundingBox.topLeft.y config.height width
            , width = width
        }


createBoundingBox : Float -> Float -> Float -> Float -> BoundingBox
createBoundingBox x y height width =
    { topLeft = { x = x, y = y }
    , topRight = { x = x + width, y = y }
    , bottomLeft = { x = x, y = y + height }
    , bottomRight = { x = x + width, y = y + height }
    }


areCollided : Collidable scope msg -> Collidable scope msg -> Bool
areCollided (Collidable c1) (Collidable c2) =
    let
        c1Xs =
            [ c1.boundingBox.bottomLeft.x, c1.boundingBox.bottomRight.x, c1.boundingBox.topLeft.x, c1.boundingBox.topRight.x ]

        c1Ys =
            [ c1.boundingBox.bottomLeft.y, c1.boundingBox.bottomRight.y, c1.boundingBox.topLeft.y, c1.boundingBox.topRight.y ]

        c2Xs =
            [ c2.boundingBox.bottomLeft.x, c2.boundingBox.bottomRight.x, c2.boundingBox.topLeft.x, c2.boundingBox.topRight.x ]

        c2Ys =
            [ c2.boundingBox.bottomLeft.y, c2.boundingBox.bottomRight.y, c2.boundingBox.topLeft.y, c2.boundingBox.topRight.y ]

        c1Left =
            c1Xs |> List.sort |> List.head

        c1Right =
            c1Xs |> List.sort |> List.reverse |> List.head

        c1Top =
            c1Ys |> List.sort |> List.head

        c1Bottom =
            c1Ys |> List.sort |> List.reverse |> List.head

        c2Left =
            c2Xs |> List.sort |> List.head

        c2Right =
            c2Xs |> List.sort |> List.reverse |> List.head

        c2Top =
            c2Ys |> List.sort |> List.head

        c2Bottom =
            c2Ys |> List.sort |> List.reverse |> List.head

        xIntersection =
            Maybe.map4
                (\c1L c1R c2L c2R ->
                    (c1L < c2R) && (c1R > c2L)
                )
                c1Left
                c1Right
                c2Left
                c2Right
                |> Maybe.withDefault False

        yIntersection =
            Maybe.map4
                (\c1T c1B c2T c2B ->
                    (c1T < c2B) && (c1B > c2T)
                )
                c1Top
                c1Bottom
                c2Top
                c2Bottom
                |> Maybe.withDefault False
    in
    xIntersection && yIntersection


pickId : Collidable scope msg -> String
pickId (Collidable config) =
    config.id


pickCollisionHandler : Collidable scope msg -> Maybe (CollisionHandler scope msg)
pickCollisionHandler (Collidable config) =
    config.onCollision
