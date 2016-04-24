module View where

import Html exposing (text)
import Color exposing (..)
import Transform2D exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)


moonExt = (400, 400)
moonRad = 155
barrelExt = (20, 29, 9)
roverExt = (66, 52, 20)

wheelSize = 17
wheelRotSpeed = 80
shakeSpeed = 250
wheelOffsL = (-20, -7)
wheelOffsR = (10, -5)

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
  let (w, h) = moonExt in
  collage w h [
      img "moon" w h |> toForm
    , group (List.map (barrel 0 0) state.barrels)
    , rover state ]

view state (w, h) =
    container w h midLeft (scene state)
