module Example.Example exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Collidable exposing (Collidable)
import CollisionDetector exposing (CollisionDetector)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json
import Process
import Task exposing (Task)
import Utils.Update as UtilsUpdate


type Msg
    = IterateCollisionDetection
    | AddCollidable CollidableType Float Float Float Float
    | UpdateCollidablePosition CollidableType Float Float
    | Error ErrorType String


type ErrorType
    = ElementNotFound


type CollidableType
    = Square1
    | Square2
    | Path1
    | Path2
    | Path3
    | Path4
    | Path5


collidableTypeToId : CollidableType -> String
collidableTypeToId collidableType =
    case collidableType of
        Square1 ->
            "square1"

        Square2 ->
            "square2"

        Path1 ->
            "path1"

        Path2 ->
            "path2"

        Path3 ->
            "path3"

        Path4 ->
            "path4"

        Path5 ->
            "path5"


type alias Model =
    { collisionDetector : CollisionDetector CollisionDetectorScope Msg
    , collisionDetectorScope : CollisionDetectorScope
    }


type alias CollisionDetectorScope =
    { square2Attributes : List (Html.Attribute Msg)
    , path1Attributes : List (Html.Attribute Msg)
    , path2Attributes : List (Html.Attribute Msg)
    , path3Attributes : List (Html.Attribute Msg)
    , path4Attributes : List (Html.Attribute Msg)
    , path5Attributes : List (Html.Attribute Msg)
    , message : String
    }


initialModel : Model
initialModel =
    { collisionDetector = CollisionDetector.create
    , collisionDetectorScope =
        { square2Attributes = []
        , path1Attributes = []
        , path2Attributes = []
        , path3Attributes = []
        , path4Attributes = []
        , path5Attributes = []
        , message = ""
        }
    }


view : Model -> Document Msg
view model =
    { title = "Example"
    , body =
        [ div [ style "height" "100vh", style "overflow" "scroll", style "position" "relative", onScroll IterateCollisionDetection ]
            [ div [ style "display" "flex", style "flex-flow" "column", style "min-height" "300vh" ]
                [ h1 [ style "position" "fixed", style "left" "0", style "top" "0" ] [ text model.collisionDetectorScope.message ]
                , square1
                , path (collidableTypeToId Path1) model.collisionDetectorScope.path1Attributes
                , path (collidableTypeToId Path2) model.collisionDetectorScope.path2Attributes
                , box2 model.collisionDetectorScope.square2Attributes
                , path (collidableTypeToId Path3) model.collisionDetectorScope.path3Attributes
                , path (collidableTypeToId Path4) model.collisionDetectorScope.path4Attributes
                , path (collidableTypeToId Path5) model.collisionDetectorScope.path5Attributes
                ]
            ]
        ]
    }


onScroll : msg -> Attribute msg
onScroll msg =
    on "scroll" (Json.succeed msg)


square1 : Html Msg
square1 =
    div
        [ id (collidableTypeToId Square1)
        , style "height" "100px"
        , style "width" "100px"
        , style "position" "fixed"
        , style "left" "calc(50% - 50px)"
        , style "top" "100px"
        , style "background-color" "red"
        , style "z-index" "10"
        , style "transform" "rotate(45deg)"
        ]
        []


box2 : List (Html.Attribute Msg) -> Html Msg
box2 attributes =
    div
        (List.append attributes
            [ id (collidableTypeToId Square2)
            , style "height" "100px"
            , style "width" "100px"
            , style "left" "50%"
            , style "background-color" "blue"
            , style "margin" "0 auto"
            ]
        )
        []


path : String -> List (Html.Attribute Msg) -> Html Msg
path idValue attributes =
    div
        (List.append attributes
            [ id idValue
            , style "width" "20px"
            , style "height" "100px"
            , style "background-color" "black"
            , style "margin" "15vh auto"
            ]
        )
        []


subscriptions model =
    Sub.batch []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IterateCollisionDetection ->
            model
                |> UtilsUpdate.withCmds
                    [ Task.attempt (resultToUpdateCollidablePosition Square1) (Dom.getElement (collidableTypeToId Square1))
                    , Task.attempt (resultToUpdateCollidablePosition Square2) (Dom.getElement (collidableTypeToId Square2))
                    , Task.attempt (resultToUpdateCollidablePosition Path1) (Dom.getElement (collidableTypeToId Path1))
                    , Task.attempt (resultToUpdateCollidablePosition Path2) (Dom.getElement (collidableTypeToId Path2))
                    , Task.attempt (resultToUpdateCollidablePosition Path3) (Dom.getElement (collidableTypeToId Path3))
                    , Task.attempt (resultToUpdateCollidablePosition Path4) (Dom.getElement (collidableTypeToId Path4))
                    , Task.attempt (resultToUpdateCollidablePosition Path5) (Dom.getElement (collidableTypeToId Path5))
                    ]
                |> CollisionDetector.cycleWithCmds model.collisionDetector

        AddCollidable collidableType x y w h ->
            model
                |> addCollidable collidableType x y w h
                |> UtilsUpdate.withCmds []

        UpdateCollidablePosition collidableType x y ->
            model
                |> updateCollisionDetector (CollisionDetector.updatePosition (collidableTypeToId collidableType) x y)
                |> UtilsUpdate.withoutCmds

        Error ElementNotFound id ->
            model
                |> UtilsUpdate.withoutCmds


addCollidable : CollidableType -> Float -> Float -> Float -> Float -> Model -> Model
addCollidable collidableType x y w h model =
    { model | collisionDetector = CollisionDetector.addCollidable (collidable collidableType x y w h) model.collisionDetector }


collidable : CollidableType -> Float -> Float -> Float -> Float -> Collidable CollisionDetectorScope Msg
collidable collidableType x y w h =
    case collidableType of
        Square1 ->
            Collidable.create (collidableTypeToId collidableType) x y w h
                |> Collidable.onCollision square1Collision

        Square2 ->
            Collidable.create (collidableTypeToId collidableType) x y w h
                |> Collidable.onCollision square2Collision

        Path1 ->
            Collidable.create (collidableTypeToId collidableType) x y w h
                |> Collidable.onCollision path1Collision

        Path2 ->
            Collidable.create (collidableTypeToId collidableType) x y w h
                |> Collidable.onCollision path2Collision

        Path3 ->
            Collidable.create (collidableTypeToId collidableType) x y w h
                |> Collidable.onCollision path3Collision

        Path4 ->
            Collidable.create (collidableTypeToId collidableType) x y w h
                |> Collidable.onCollision path4Collision

        Path5 ->
            Collidable.create (collidableTypeToId collidableType) x y w h
                |> Collidable.onCollision path5Collision


square1Collision : Collidable CollisionDetectorScope Msg -> Collidable CollisionDetectorScope Msg -> CollisionDetectorScope -> ( CollisionDetectorScope, Cmd msg )
square1Collision me collided scope =
    scope
        |> UtilsUpdate.withoutCmds


square2Collision : Collidable CollisionDetectorScope Msg -> Collidable CollisionDetectorScope Msg -> CollisionDetectorScope -> ( CollisionDetectorScope, Cmd msg )
square2Collision me collided scope =
    { scope
        | square2Attributes = [ style "border-radius" "50px" ]
        , message = "square collided"
    }
        |> UtilsUpdate.withoutCmds


path1Collision : Collidable CollisionDetectorScope Msg -> Collidable CollisionDetectorScope Msg -> CollisionDetectorScope -> ( CollisionDetectorScope, Cmd msg )
path1Collision me collided scope =
    { scope
        | path1Attributes = [ style "opacity" "20%" ]
        , message = "path1 collided"
    }
        |> UtilsUpdate.withoutCmds


path2Collision : Collidable CollisionDetectorScope Msg -> Collidable CollisionDetectorScope Msg -> CollisionDetectorScope -> ( CollisionDetectorScope, Cmd msg )
path2Collision me collided scope =
    { scope
        | path2Attributes = [ style "opacity" "20%" ]
        , message = "path2 collided"
    }
        |> UtilsUpdate.withoutCmds


path3Collision : Collidable CollisionDetectorScope Msg -> Collidable CollisionDetectorScope Msg -> CollisionDetectorScope -> ( CollisionDetectorScope, Cmd msg )
path3Collision me collided scope =
    { scope
        | path3Attributes = [ style "opacity" "20%" ]
        , message = "path3 collided"
    }
        |> UtilsUpdate.withoutCmds


path4Collision : Collidable CollisionDetectorScope Msg -> Collidable CollisionDetectorScope Msg -> CollisionDetectorScope -> ( CollisionDetectorScope, Cmd msg )
path4Collision me collided scope =
    { scope
        | path4Attributes = [ style "opacity" "20%" ]
        , message = "path4 collided"
    }
        |> UtilsUpdate.withoutCmds


path5Collision : Collidable CollisionDetectorScope Msg -> Collidable CollisionDetectorScope Msg -> CollisionDetectorScope -> ( CollisionDetectorScope, Cmd msg )
path5Collision me collided scope =
    { scope
        | path5Attributes = [ style "opacity" "20%" ]
        , message = "path5 collided"
    }
        |> UtilsUpdate.withoutCmds


updateCollisionDetector : (CollisionDetector CollisionDetectorScope Msg -> CollisionDetector CollisionDetectorScope Msg) -> Model -> Model
updateCollisionDetector mapper model =
    { model | collisionDetector = mapper model.collisionDetector }


init : () -> ( Model, Cmd Msg )
init _ =
    initialModel
        |> UtilsUpdate.withCmds
            [ Task.attempt (resultToAddCollidable Square1) (Dom.getElement (collidableTypeToId Square1))
            , Task.attempt (resultToAddCollidable Square2) (Dom.getElement (collidableTypeToId Square2))
            , Task.attempt (resultToAddCollidable Path1) (Dom.getElement (collidableTypeToId Path1))
            , Task.attempt (resultToAddCollidable Path2) (Dom.getElement (collidableTypeToId Path2))
            , Task.attempt (resultToAddCollidable Path3) (Dom.getElement (collidableTypeToId Path3))
            , Task.attempt (resultToAddCollidable Path4) (Dom.getElement (collidableTypeToId Path4))
            , Task.attempt (resultToAddCollidable Path5) (Dom.getElement (collidableTypeToId Path5))
            ]


resultToAddCollidable : CollidableType -> Result Dom.Error Dom.Element -> Msg
resultToAddCollidable collidableType result =
    case result of
        Ok value ->
            AddCollidable collidableType value.element.x value.element.y value.element.width value.element.height

        Err (Dom.NotFound id) ->
            Error ElementNotFound id


resultToUpdateCollidablePosition : CollidableType -> Result Dom.Error Dom.Element -> Msg
resultToUpdateCollidablePosition collidableType result =
    case result of
        Ok value ->
            UpdateCollidablePosition collidableType value.element.x value.element.y

        Err (Dom.NotFound id) ->
            Error ElementNotFound id


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


loop : Float -> Task error value -> Task error ()
loop everyMillis task =
    task
        |> Task.andThen (\_ -> Process.sleep everyMillis)
        |> Task.andThen (\_ -> loop everyMillis task)
