module Main where

import Window
import Keyboard
import Text
import Signal exposing (..)

import List
import Time exposing (..)

import View
import Route
import Model exposing (..)

tickTime = 0.02
moveSpeed = 0.1


type Update = Tick Float
updates =
  mergeMany
  [ map Tick (Time.every (Time.second*tickTime)) ]

updMove state dt n =
  let
    dp = state.dir*dt*moveSpeed
    fuel = state.fuel - dp*fuelConsumption
    pos = state.pos - dp
  in
    {state | pos = pos, fuel = max fuel 0}

--interpState st1 st2 action t =
--  case action of
--    Move n -> updMove st1 dt n
--    Load n -> state
--    Fill n -> state
--    Wait n -> state
--    Pick n -> state
--    Dump -> state

advance state dt =
  let
    action = Route.route |> List.drop state.step |> List.head
    anim = state.anim + dt
  in
    case action of
      Just (Move n) -> updMove state dt n
      Just (Load n) -> state
      Just (Fill n) -> state
      Just (Wait n) -> state
      Just (Pick n) -> state
      Just (Dump) -> state
      _ -> state


foldUpdates update state =
  case update of
    Tick _ -> advance state tickTime

main =
  map2 View.view
  (foldp foldUpdates
    {pos = 0, dir = 1, fuel = 1, spare = 1, barrels = [],
     step = 0, anim = 1} updates)
  Window.dimensions
