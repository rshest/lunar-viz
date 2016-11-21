module Main exposing (..)

import Html exposing (Html)
import Time exposing (..)
import View exposing (..)
import Anim exposing (RoverAnim, advance)
import Constants exposing (tickTime)


actions : Mailbox Update
actions =
    mailbox NoOp


updates : Signal Update
updates =
    mergeMany [ map Tick (Time.every (Time.second * tickTime)), actions.signal ]


foldUpd : Update -> RoverAnim -> RoverAnim
foldUpd update anim =
    case update of
        Tick _ ->
            Anim.advance anim tickTime

        JumpToStep n ->
            Anim.jumpToStep anim n

        NoOp ->
            anim


main : Html Msg
main =
    map (view actions.address) (foldp foldUpd Anim.init updates)
