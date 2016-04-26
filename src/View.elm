module View where

import Constants exposing (..)
import Model exposing(Rover)
import Anim exposing(RoverAnim)

import Color exposing (..)
import Transform2D exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)


img : String -> Int -> Int -> Element
img name w h = image w h ("img/" ++ name ++ ".png")


--  moves, and _then_ rotates a form
moveRot : (Float, Float) -> Float -> Form -> Form
moveRot offs rot f =
    [f |> move offs] |> group |> rotate rot

-- displays a barrel
barrel : (Float, Float) -> Float -> Form
barrel offs fuel =
  let (w, h, lid) = barrelExt
      hf = (h - lid)*fuel
  in
    [
      rect (w - 4) hf |> filled orange |> moveY ((hf - h)*0.5 + 2)
    , img "barrel" w h |> toForm
    --, show "100%" |> toForm |>  moveY 25
    ] |> group |> move offs


-- displays a barrel standing on the ground
barrel_ground : (Float, Float) -> Form
barrel_ground (pos, fuel) =
  barrel (0, 0) fuel |> moveRot (0, moonRad) (pos*2*pi)


-- displays a wheel
wheel : (Float, Float) -> Float -> Form
wheel offs rot =
  img "wheel" wheelSize wheelSize |> toForm |>
    move offs |> rotate (rot*wheelRotSpeed)


--  displays the vehicle (rover)
vehicle : Rover -> Form
vehicle {pos, dir, fuel, spare} =
  let (w, h, hoffs) = roverExt
      wheelm = pos*dir
      shake = sin((Debug.watch "pos" pos)*shakeSpeed)
  in [
      if spare >= 0 then
        barrel (fst spareOffs, snd spareOffs - shake) spare
      else group []
    , barrel (fst tankOffs, snd tankOffs + shake) fuel
    , img "rover" w h |> toForm |>  moveY (hoffs + shake)
    , wheel wheelOffsL (wheelm + 1.5)
    , wheel wheelOffsR (wheelm + 0.1)
    ] |> groupTransform (scaleX -dir) |> moveRot (0, moonRad) (pos*2*pi)


-- full scene display
scene : RoverAnim -> (Int, Int) -> Element
scene anim (w, h) =
  let (mw, mh) = moonExt
      {rover} = Anim.interp anim
  in
  container w h midLeft
  (collage mw mh [
      img "moon" mw mh |> toForm
    , group (List.map barrel_ground rover.barrels)
    , vehicle rover])
