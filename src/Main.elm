module Main where

import Window
import Keyboard
import Text

import Html exposing (text)

import Signal exposing (..)
import Time exposing (..)
import Color exposing (..)
import Transform2D exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

type Action =
    Move Float  -- move n units (backward if negative)
  | Load Float  -- load n units from the nearby tank
  | Fill Float  -- fill n units from the spare tank
  | Dump        -- dump the spare tank on the ground

route = [
  [Move -0.1, Dump, Move 0.1],
  [Move 0.1, Dump, Move -0.1],
  [Move -0.1, Load 0.5, Move -0.1, Dump, Move 0.1, Load 0.5, Move 0.1],
  [Move 0.1, Load 0.5, Move 0.1, Dump, Move -0.1, Load 0.5, Move -0.1]]

barrels = [(0.1, 0.5), (0.2, 0.5), (0.3, 0.25), (0.7, 0.75),
  (0.3, 1), (0.9, 1), (0.5, 1), (0.25, 0.6), (0.75, 0.6)]
moonRad = 155

img name w h = image w h ("img/" ++ name ++ ".png")

barrel offsX offsY (pos, fillRatio) =
  let
    (w, h) = (20, 29)
    hf = (h - 9)*fillRatio
    g = [
      rect (w - 4) hf |> filled orange |> moveY ((hf - h)*0.5 + 2)
    , img "barrel" w h |> toForm
    --, show "100%" |> toForm |>  moveY (moonRad + 25)
    ] |> group |> moveY moonRad
  in
    [g] |> group |> move (offsX, offsY) |> rotate (pos*2*pi)

wheel offsX offsY rot =
  img "wheel" 17 17 |> toForm |>
    move (offsX, offsY) |>
    rotate (rot*800) |> moveY moonRad

rover pos dir fillRatio spareFill =
  let wheelm = pos*0.1*dir
      gait = sin(pos*250)
  in [
      barrel -20 (23 + gait) (0, fillRatio),
      if spareFill >= 0 then barrel 26 (12 - gait) (0, spareFill) else group []
    , img "rover" 66 52 |> toForm |>  moveY (moonRad + 20 + gait)
    , wheel -20 -7 (wheelm + 1.5), wheel 10 -5 (wheelm + 0.1)
    ] |> groupTransform (scaleX dir) |> rotate (pos*2*pi*dir)

lunarScene roverPos barrelPos =
  collage 400 400 [
      img "moon" 400 400 |> toForm
    , group (List.map (barrel 0 0) barrelPos)
    , rover -roverPos 1 0.5 1
  ]

view roverPos (w, h) =
    container w h midLeft (lunarScene roverPos barrels)

type Update = Tick Float
updates =
  mergeMany
  [ map Tick (Time.every (Time.second*0.01))
  ]

foldUpdates update roverPos =
  case update of
    Tick _ -> roverPos + 0.01*0.1

main =
  map2 view
  (foldp foldUpdates 0 updates)
  Window.dimensions
