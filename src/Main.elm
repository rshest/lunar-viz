module Main where

import Window
import Keyboard
import Text
import Signal exposing (..)

import Html exposing (text)

import List
import Time exposing (..)
import Color exposing (..)
import Transform2D exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

type Action =
    Move Float  -- move n units (backward if negative)
  | Load Float  -- load n units from the nearby tank
  | Fill Float  -- fill n units from the spare tank
  | Wait Float  -- wait n seconds
  | Pick Float  -- pick a spare tank with n units of fuel
  | Dump        -- dump the spare tank on the ground

route = [Move 20,
  Load 100, Pick 100, Move -10, Dump, Move 10,
  Load 100, Pick 100, Move 10, Dump, Move -10,
  Load 100, Pick 100, Move -10, Load 50, Move -10, Dump, Move 10, Load 50, Move 10,
  Load 100, Pick 100, Move 10, Load 50, Move 10, Dump, Move -10, Load 50, Move -10,
  Load 100, Pick 100, Move 10000000]

moonRad = 155
tickTime = 0.02
barrelExt = (20, 29, 9)
wheelSize = 17
wheelRotSpeed = 80
shakeSpeed = 250
wheelOffsL = (-20, -7)
wheelOffsR = (10, -5)
roverExt = (66, 52, 20)
moveSpeed = 0.1
fuelConsumption = 5

img name w h = image w h ("img/" ++ name ++ ".png")

barrel offsX offsY (pos, fuel) =
  let
    (w, h, lid) = barrelExt
    hf = (h - lid)*fuel
    g = [
      rect (w - 4) hf |> filled orange |> moveY ((hf - h)*0.5 + 2)
    , img "barrel" w h |> toForm
    --, show "100%" |> toForm |>  moveY (moonRad + 25)
    ] |> group |> moveY moonRad
  in
    [g] |> group |> move (offsX, offsY) |> rotate (pos*2*pi)

wheel offs rot =
  img "wheel" wheelSize wheelSize |> toForm |>
    move offs |>
    rotate (rot*wheelRotSpeed) |> moveY moonRad

rover {pos, dir, fuel, spare} =
  let (w, h, hoffs) = roverExt
      wheelm = pos*dir
      shake = sin(pos*shakeSpeed)
  in [
      barrel -20 (23 + shake) (0, fuel),
      if spare >= 0 then barrel 26 (12 - shake) (0, spare) else group []
    , img "rover" w h |> toForm |>  moveY (moonRad + hoffs + shake)
    , wheel wheelOffsL (wheelm + 1.5), wheel wheelOffsR (wheelm + 0.1)
    ] |> groupTransform (scaleX dir) |> rotate (pos*2*pi*dir)

scene state =
  collage 400 400 [
      img "moon" 400 400 |> toForm
    , group (List.map (barrel 0 0) state.barrels)
    , rover state ]

view state (w, h) =
    container w h midLeft (scene state)

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
    action = route |> List.drop state.step |> List.head
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
  map2 view
  (foldp foldUpdates
    {pos = 0, dir = 1, fuel = 1, spare = 1, barrels = [],
     step = 0, anim = 1} updates)
  Window.dimensions
