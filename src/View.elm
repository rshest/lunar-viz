module View where

import Constants exposing (..)
import Model exposing(Rover)

import Color exposing (..)
import Transform2D exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)


img : String -> Int -> Int -> Element
img name w h = image w h ("img/" ++ name ++ ".png")


-- displays a barrel
barrel : (Float, Float) -> (Float, Float) -> Form
barrel offs (pos, fuel) =
  let
    (w, h, lid) = barrelExt
    hf = (h - lid)*fuel
    g = [
      rect (w - 4) hf |> filled orange |> moveY ((hf - h)*0.5 + 2)
    , img "barrel" w h |> toForm
    --, show "100%" |> toForm |>  moveY (moonRad + 25)
    ] |> group |> moveY moonRad
  in
    [g] |> group |> move offs |> rotate (pos*2*pi)


-- displays a wheel
wheel : (Float, Float) -> Float -> Form
wheel offs rot =
  img "wheel" wheelSize wheelSize |> toForm |>
    move offs |>
    rotate (rot*wheelRotSpeed) |> moveY moonRad


--  displays the vehicle (rover)
vehicle : Rover -> Form
vehicle {pos, dir, fuel, spare} =
  let (w, h, hoffs) = roverExt
      wheelm = pos*dir
      shake = sin(pos*shakeSpeed)
  in [
      if spare >= 0 then
        barrel (fst spareOffs, snd spareOffs - shake) (0, spare)
      else group []
    , barrel (fst tankOffs, snd tankOffs + shake) (0, fuel)
    , img "rover" w h |> toForm |>  moveY (moonRad + hoffs + shake)
    , wheel wheelOffsL (wheelm + 1.5)
    , wheel wheelOffsR (wheelm + 0.1)
    ] |> groupTransform (scaleX dir) |> rotate (pos*2*pi*dir)


-- full scene display
scene : (Rover, Int, Float) -> (Int, Int) -> Element
scene (rover, step, t) (w, h) =
  let (mw, mh) = moonExt in
  container w h midLeft
  (collage mw mh [
      img "moon" mw mh |> toForm
    , group (List.map (barrel (0, 0)) rover.barrels)
    , vehicle rover])
