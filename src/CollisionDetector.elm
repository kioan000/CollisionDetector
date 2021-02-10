module CollisionDetector exposing
    ( CollisionDetector
    , Msg
    , addCollidable
    , collisionDetector
    , getCollidable
    , tickEvent
    , tickOnAnimationFrame
    , update
    , viewBoundingBox
    )

import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Collidable as Collidable exposing (Collidable)
import Collidable.BoundingBox as BoundingBox
import Collidable.Point as Point exposing (Point)
import Dict exposing (Dict)
import Html exposing (Html)
import Task
import Utils.Cmd as Cmd
import Utils.Render as Render
import Utils.Update as PH


type CollisionDetector msg
    = CollisionDetector (CDConfig msg)


type alias CDConfig msg =
    { elementsThree : Dict String (Collidable msg)
    , internalMsgTagger : Msg msg -> msg
    , errorMsg : Dom.Error -> msg
    }


type Msg msg
    = ElementUpdated String Dom.Element
    | AddElement Dom.Element (Collidable msg)
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
        ElementUpdated id element ->
            model
                |> updateHostModel (updatePosition id (Point.point element.element.x element.element.y) element.element.height element.element.width)
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
                        >> findCollisions
                        >> List.map emitCollision
                        >> Cmd.batch
                    ]

        AddElement element collidable ->
            model
                |> updateHostModel (insertCollidable (Collidable.track (Point.point element.element.x element.element.y) element.element.height element.element.width collidable))
                |> PH.withoutCmds


updatePosition : String -> Point -> Float -> Float -> CollisionDetector msg -> CollisionDetector msg
updatePosition id topLeft height width (CollisionDetector config) =
    CollisionDetector { config | elementsThree = Dict.update id (Maybe.map (Collidable.updateBoundingBox topLeft height width)) config.elementsThree }


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


addCollidable : Collidable msg -> CollisionDetector msg -> Cmd msg
addCollidable collidable cD =
    Task.attempt
        (addElementOrError (pickInternalMsgTagger cD) (pickErrorMsg cD) collidable)
        (Dom.getElement (Collidable.pickId collidable))


updateElementOrError : (Msg msg -> msg) -> (Dom.Error -> msg) -> String -> Result Dom.Error Dom.Element -> msg
updateElementOrError internalMsgTagger errorTagger id result =
    case result of
        Result.Ok element ->
            ElementUpdated id element
                |> internalMsgTagger

        Result.Err domNotFoundErr ->
            domNotFoundErr
                |> errorTagger


addElementOrError : (Msg msg -> msg) -> (Dom.Error -> msg) -> Collidable msg -> Result Dom.Error Dom.Element -> msg
addElementOrError internalMsgTagger errorTagger collidable result =
    case result of
        Result.Ok element ->
            AddElement element collidable
                |> internalMsgTagger

        Result.Err domNotFoundErr ->
            domNotFoundErr
                |> errorTagger


updateHostModel : (CollisionDetector msg -> CollisionDetector msg) -> HostModel model msg -> HostModel model msg
updateHostModel cDMorph hM =
    { hM
        | collisionDetector = cDMorph hM.collisionDetector
    }


getCollidable : String -> CollisionDetector msg -> Maybe (Collidable msg)
getCollidable id (CollisionDetector { elementsThree }) =
    Dict.get id elementsThree


viewBoundingBox : String -> CollisionDetector msg -> Html msg
viewBoundingBox id =
    getCollidable id
        >> Maybe.andThen Collidable.pickBoundingBox
        >> Render.maybeMap (BoundingBox.view [])
