module Utils.Update exposing (addCmd, withCmds, withCmdsMap, withoutCmds)


addCmd : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addCmd cmdToAdd ( model, cmds ) =
    ( model, Cmd.batch [ cmds, cmdToAdd ] )


withCmds : List (Cmd msg) -> model -> ( model, Cmd msg )
withCmds cmds model =
    ( model, Cmd.batch cmds )


withCmdsMap : List (model -> Cmd msg) -> model -> ( model, Cmd msg )
withCmdsMap cmds model =
    ( model, Cmd.batch (List.map ((|>) model) cmds) )


withoutCmds : model -> ( model, Cmd msg )
withoutCmds model =
    ( model, Cmd.none )
