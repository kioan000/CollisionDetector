module Utils.Update exposing (withCmds, withCmdsMap, withoutCmds)


withCmds : List (Cmd msg) -> model -> ( model, Cmd msg )
withCmds cmds model =
    ( model, Cmd.batch cmds )


withCmdsMap : List (model -> Cmd msg) -> model -> ( model, Cmd msg )
withCmdsMap cmds model =
    ( model, Cmd.batch (List.map ((|>) model) cmds) )


withoutCmds : model -> ( model, Cmd msg )
withoutCmds model =
    ( model, Cmd.none )
