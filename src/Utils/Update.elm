module Utils.Update exposing (..)


addCmd : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addCmd cmdToAdd ( model, cmds ) =
    ( model, Cmd.batch [ cmds, cmdToAdd ] )


withCmds : List (Cmd msg) -> model -> ( model, Cmd msg )
withCmds cmds model =
    ( model, Cmd.batch cmds )


withoutCmds : model -> ( model, Cmd msg )
withoutCmds model =
    ( model, Cmd.none )
