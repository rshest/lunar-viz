module Main where

import Window
import Html exposing (Html)
import Signal exposing (..)

import Time exposing (..)

import View
import Anim exposing (RoverAnim, advance)

import Constants exposing (..)

--port locationSearch : String

type Update = Tick Float

updates : Signal Update
updates =
  mergeMany [ map Tick (Time.every (Time.second*tickTime)) ]


foldUpd : Update -> RoverAnim -> RoverAnim
foldUpd update anim =
  case update of
    Tick _ ->
      Anim.advance anim tickTime

main : Signal Html
main =
  map2 View.scene
  (foldp foldUpd Anim.init updates)
  Window.dimensions
