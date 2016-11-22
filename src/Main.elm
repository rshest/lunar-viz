module Main exposing (..)

import Html exposing (Html)
import Time exposing (..)
import View exposing (..)
import Msg  exposing (..)
import Anim exposing (RoverAnim, advance)
import Constants exposing (tickTime)

subs: RoverAnim -> Sub Msg
subs anim =
  Time.every (Time.second * tickTime) Tick


update : Msg -> RoverAnim -> (RoverAnim, Cmd Msg)
update msg anim =
  case msg of
    Tick _ ->
      (Anim.advance anim tickTime, Cmd.none)

    JumpToStep n ->
      (Anim.jumpToStep anim n, Cmd.none)


--main : Html Msg
main =
  Html.program
    { init = Anim.init
    , update = update
    , view = view
    , subscriptions = subs
    }