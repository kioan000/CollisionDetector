module CollisionDetector exposing
    ( CollisionDetector
    , Msg
    , addCollidable
    , collisionDetector
    , tickEvent
    , tickOnAnimationFrame
    , update
    )

import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Collidable as Collidable exposing (Collidable)
import Dict exposing (Dict)
import Task
import Utils.Cmd as Cmd
import Utils.Update as PH


type CollisionDetector msg
    = CollisionDetector (CDConfig msg)


type alias CDConfig msg =
    { elementsThree : Dict String (Collidable msg)
    , internalMsgTagger : Msg msg -> msg
    , errorMsg : Dom.Error -> msg
    }


type Msg msg
    = ElementUpdated String Float Float
    | AddElement String Float Float Float Float (Maybe (Collidable.CollisionEvent msg))
    | Tick


{-| Use tick event to emit tick event on explicit moment (eg. on scroll).

  - Don't use with tickOnAnimationFrame

-}
tickEvent : CollisionDetector msg -> msg
tickEvent (CollisionDetector config) =
    config.internalMsgTagger Tick


type alias HostModel hostModel msg =
    { hostModel | collisionDetector : CollisionDetector msg }


collisionDetector : (Msg msg -> msg) -> (Dom.Error -> msg) -> CollisionDetector msg
collisionDetector internalMsgTagger errorMsg =
    CollisionDetector
        { elementsThree = Dict.empty
        , internalMsgTagger = internalMsgTagger
        , errorMsg = errorMsg
        }


insertCollidable : Collidable msg -> CollisionDetector msg -> CollisionDetector msg
insertCollidable collidable (CollisionDetector config) =
    CollisionDetector
        { config
            | elementsThree = Dict.insert (Collidable.pickId collidable) collidable config.elementsThree
        }


{-| Add this subscription to your app if you want to emit tick event at 60fps accordingly with browser repaint cycle.

  - Don't use with explicit tickEvent handling

-}
tickOnAnimationFrame : CollisionDetector msg -> Sub msg
tickOnAnimationFrame =
    tickEvent
        >> always
        >> BrowserEvents.onAnimationFrame


update : Msg msg -> HostModel hostModel msg -> ( HostModel hostModel msg, Cmd msg )
update msg model =
    case msg of
        ElementUpdated id x y ->
            model
                |> updateHostModel (updatePosition id x y)
                |> PH.withoutCmds

        Tick ->
            model
                |> PH.withCmdsMap
                    [ .collisionDetector
                        >> pickCollidablesId
                        >> List.map (\id -> trackCollidable id model.collisionDetector)
                        >> Cmd.batch
                    , .collisionDetector
                        >> pickCollidables
                        -->> Debug.log "collidables: "
                        >> findCollisions
                        -->> Debug.log "collisions: "
                        >> List.map emitCollision
                        >> Cmd.batch
                    ]

        AddElement id x y height width collisionHandler ->
            model
                |> updateHostModel (insertCollidable (Collidable.collidable id x y height width collisionHandler))
                |> PH.withoutCmds


updatePosition : String -> Float -> Float -> CollisionDetector msg -> CollisionDetector msg
updatePosition id x y (CollisionDetector config) =
    CollisionDetector { config | elementsThree = Dict.update id (Maybe.map (Collidable.setPosition x y)) config.elementsThree }


findCollisions : List (Collidable msg) -> List ( Collidable msg, Collidable msg )
findCollisions cD =
    case cD of
        x1 :: x2 :: xs ->
            if Collidable.areCollided x1 x2 then
                ( x1, x2 )
                    :: List.append
                        (findCollisions (x1 :: xs))
                        (findCollisions (x2 :: xs))

            else
                List.append
                    (findCollisions (x1 :: xs))
                    (findCollisions (x2 :: xs))

        _ ->
            []


emitCollision : ( Collidable msg, Collidable msg ) -> Cmd msg
emitCollision ( c1, c2 ) =
    Cmd.batch
        [ Collidable.pickCollisionEvent c1
            |> Maybe.map (\cMsg -> cMsg c1 c2 |> Cmd.fromMsg)
            |> Maybe.withDefault Cmd.none
        , Collidable.pickCollisionEvent c2
            |> Maybe.map (\cMsg -> cMsg c2 c1 |> Cmd.fromMsg)
            |> Maybe.withDefault Cmd.none
        ]


pickCollidables : CollisionDetector msg -> List (Collidable msg)
pickCollidables (CollisionDetector config) =
    Dict.values config.elementsThree


pickCollidablesId : CollisionDetector msg -> List String
pickCollidablesId (CollisionDetector config) =
    Dict.keys config.elementsThree


pickCollisionHandlers : CollisionDetector msg -> List (Collidable.CollisionEvent msg)
pickCollisionHandlers =
    pickCollidables >> List.map Collidable.pickCollisionEvent >> List.filterMap identity


pickErrorMsg : CollisionDetector msg -> (Dom.Error -> msg)
pickErrorMsg (CollisionDetector config) =
    config.errorMsg


pickInternalMsgTagger : CollisionDetector msg -> (Msg msg -> msg)
pickInternalMsgTagger (CollisionDetector config) =
    config.internalMsgTagger


trackCollidable : String -> CollisionDetector msg -> Cmd msg
trackCollidable collidableId cD =
    Task.attempt
        (updateElementOrError (pickInternalMsgTagger cD) (pickErrorMsg cD) collidableId)
        (Dom.getElement collidableId)


addCollidable : String -> Maybe (Collidable.CollisionEvent msg) -> CollisionDetector msg -> Cmd msg
addCollidable collidableId collisionEvent cD =
    Task.attempt
        (addElementOrError collisionEvent (pickInternalMsgTagger cD) (pickErrorMsg cD) collidableId)
        (Dom.getElement collidableId)


updateElementOrError : (Msg msg -> msg) -> (Dom.Error -> msg) -> String -> Result Dom.Error Dom.Element -> msg
updateElementOrError internalMsgTagger errorTagger id result =
    case result of
        Result.Ok element ->
            ElementUpdated id element.element.x element.element.y
                |> internalMsgTagger

        Result.Err domNotFoundErr ->
            domNotFoundErr
                |> errorTagger


addElementOrError : Maybe (Collidable.CollisionEvent msg) -> (Msg msg -> msg) -> (Dom.Error -> msg) -> String -> Result Dom.Error Dom.Element -> msg
addElementOrError collisionEvent internalMsgTagger errorTagger id result =
    case result of
        Result.Ok element ->
            AddElement id element.element.x element.element.y element.element.height element.element.width collisionEvent
                |> internalMsgTagger

        Result.Err domNotFoundErr ->
            domNotFoundErr
                |> errorTagger


updateHostModel : (CollisionDetector msg -> CollisionDetector msg) -> HostModel model msg -> HostModel model msg
updateHostModel cDMorph hM =
    { hM
        | collisionDetector = cDMorph hM.collisionDetector
    }
