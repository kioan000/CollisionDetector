module Utils.Cmd exposing (fromMsg)

import Task


fromMsg : msg -> Cmd msg
fromMsg =
    Task.perform identity << Task.succeed
