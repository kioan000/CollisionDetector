module Example.Example exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Collidable as Collidable exposing (Collidable)
import Collidable.BoundingBox as BoundingBox
import CollisionDetector as CollisionDetector exposing (CollisionDetector)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json
import Task
import Utils.Update as UtilsUpdate


type Msg
    = SquareCollision (Collidable Msg) (Collidable Msg)
    | CollisionDetectorMsg (CollisionDetector.Msg Msg)
    | CollisionDetectorError Dom.Error
    | Track (Result Dom.Error Dom.Element)
    | Tick


type alias Model =
    { collisionDetector : CollisionDetector Msg
    , message : String
    , attrs : List (Html.Attribute Msg)
    , x : Float
    , y : Float
    }


initialModel : Model
initialModel =
    { collisionDetector =
        CollisionDetector.collisionDetector
            CollisionDetectorMsg
            CollisionDetectorError
    , message = ""
    , attrs = []
    , x = 0
    , y = 0
    }


view : Model -> Document Msg
view model =
    { title = "Example"
    , body =
        [ div [ style "height" "100vh", style "overflow" "scroll", style "position" "relative", onScroll Tick ]
            [ div [ style "display" "flex", style "flex-flow" "column", style "min-height" "300vh", style "padding-top" "200px" ]
                [ h1 [ style "position" "fixed", style "left" "0", style "top" "0" ] [ text model.message ]
                , square1

                --, model.collisionDetector
                --  |> CollisionDetector.viewBoundingBox "square1"
                , path "path1" []

                --, model.collisionDetector
                -- |> CollisionDetector.viewBoundingBox "path1"
                , path "path2" []
                , box2 model.attrs
                , path "path3" []
                , path "path4" []
                , path "path5" []
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
        [ id "square1"
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
            [ id "square2"
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SquareCollision square collidable ->
            model
                |> setMessage (Collidable.pickId square ++ " collided with " ++ Collidable.pickId collidable)
                |> UtilsUpdate.withoutCmds

        CollisionDetectorMsg cdMsg ->
            model
                |> CollisionDetector.update cdMsg

        CollisionDetectorError error ->
            model
                |> UtilsUpdate.withoutCmds

        Track (Result.Err _) ->
            model
                |> setMessage "Error"
                |> UtilsUpdate.withoutCmds

        Track (Result.Ok element) ->
            model
                |> setMessage ("(x,y): (" ++ String.fromFloat element.element.x ++ "," ++ String.fromFloat element.element.y ++ ")")
                |> UtilsUpdate.withoutCmds

        Tick ->
            model
                |> UtilsUpdate.withCmds
                    [ Task.attempt Track (Dom.getElement "square1")
                    ]


setMessage : String -> Model -> Model
setMessage msg model =
    { model | message = msg }


init : () -> ( Model, Cmd Msg )
init _ =
    initialModel
        |> UtilsUpdate.withCmds
            [ --.collisionDetector >> CollisionDetector.addCollidable "square1" Nothing
              --, .collisionDetector >> CollisionDetector.addCollidable "path1" Nothing
              --, .collisionDetector >> CollisionDetector.addCollidable "path2" Nothing
              --, .collisionDetector >> CollisionDetector.addCollidable "square2" Nothing
              --, .collisionDetector >> CollisionDetector.addCollidable "path3" Nothing
              --, .collisionDetector >> CollisionDetector.addCollidable "path4" Nothing
              --, .collisionDetector >> CollisionDetector.addCollidable "path5" Nothing
              Task.attempt Track (Dom.getElement "square1")
            ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
