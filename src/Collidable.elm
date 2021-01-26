module Collidable exposing
    ( Collidable
    , CollisionEvent
    , areCollided
    , collidable
    , pickCollisionEvent
    , pickId
    , setHeight
    , setPosition
    , setWidth
    )


type Collidable msg
    = Collidable (Config msg)


type alias Config msg =
    { id : String
    , boundingBox : BoundingBox
    , collisionBoxType : CollisionBoxType
    , collisionEvent : Maybe (CollisionEvent msg)
    , collidesWith : CollidesWithOption
    , height : Float
    , width : Float
    , unit : CoordinatesUnit
    }


{-| Type alias for collision handler function.
-}
type alias CollisionEvent msg =
    Collidable msg -> Collidable msg -> msg


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


collidable : String -> Float -> Float -> Float -> Float -> Maybe (CollisionEvent msg) -> Collidable msg
collidable id x y height width event =
    Collidable
        { id = id
        , boundingBox = createBoundingBox x y height width
        , collisionBoxType = Squared
        , collisionEvent = event
        , collidesWith = All
        , height = height
        , width = width
        , unit = Pixel
        }


setPosition : Float -> Float -> Collidable msg -> Collidable msg
setPosition x y (Collidable config) =
    Collidable
        { config
            | boundingBox = createBoundingBox x y config.height config.width
        }


setHeight : Float -> Collidable msg -> Collidable msg
setHeight height (Collidable config) =
    Collidable
        { config
            | boundingBox = createBoundingBox config.boundingBox.topLeft.x config.boundingBox.topLeft.y height config.width
            , height = height
        }


setWidth : Float -> Collidable msg -> Collidable msg
setWidth width (Collidable config) =
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


areCollided : Collidable msg -> Collidable msg -> Bool
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


pickId : Collidable msg -> String
pickId (Collidable config) =
    config.id


pickCollisionEvent : Collidable msg -> Maybe (CollisionEvent msg)
pickCollisionEvent (Collidable config) =
    config.collisionEvent
