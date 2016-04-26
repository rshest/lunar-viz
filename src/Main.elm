module Main where

import Window
import Signal exposing (..)

import List
import Time exposing (..)

import Model
import View
import Anim

import Model exposing (..)
import Constants exposing (..)


type Update = Tick Float


updates =
  mergeMany
  [ map Tick (Time.every (Time.second*tickTime)) ]

updMove rover dt n =
  let
    dp = rover.dir*dt*moveAnimSpeed
    fuel = rover.fuel - dp*fuelConsumption
    pos = rover.pos - dp
  in
    {rover | pos = pos, fuel = max fuel 0}


advance (rover, step, t) dt =
  let
    action = Model.planRoute |> List.drop step |> List.head
    newt = t + dt
    newr =
      case action of
        Just (Move n) -> updMove rover dt n
        Just (Load n) -> rover
        Just (Fill n) -> rover
        Just (Pick n) -> rover
        Just (Dump) -> rover
        _ -> rover
  in (newr, step, t)


foldUpdates update state =
  case update of
    Tick _ -> advance state tickTime


port locationSearch : String

main =
  map2 View.scene
  (foldp foldUpdates (Model.init, 0, 1) updates)
  Window.dimensions
