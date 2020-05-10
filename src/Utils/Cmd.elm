module Utils.Cmd exposing (..)

import Task


fromMsg : msg -> Cmd msg
fromMsg =
    Task.perform identity << Task.succeed
