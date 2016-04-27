module View where

import Constants exposing (..)
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
vehicle : RoverAnim -> Form
vehicle anim =
  let (w, h, hoffs) = roverExt
      {pos, dir, fuel, spare} = anim.rover
      wheelm = pos*dir
      shake = sin((Debug.watch "pos" pos)*shakeSpeed)
      soffs = (fst anim.spareOffs, snd anim.spareOffs - shake)
  in [
      barrel (fst tankOffs, snd tankOffs + shake) fuel
    , img "rover" w h |> toForm |>  moveY (hoffs + shake)
    , wheel wheelOffsL (wheelm + (fst wheelPhase))
    , wheel wheelOffsR (wheelm + (snd wheelPhase))
    , if spare >= 0 then barrel soffs spare else group []
    ] |> groupTransform (scaleX -dir) |> moveRot (0, moonRad) (pos*2*pi)


-- full scene display
scene : RoverAnim -> (Int, Int) -> Element
scene anim (w, h) =
  let (mw, mh) = moonExt
      animInt = Anim.interp anim
      rover = animInt.rover
  in
  container w h topLeft
  (collage mw mh
    [
      img "moon" mw mh |> toForm
    , vehicle animInt
    , group (List.map barrel_ground rover.barrels)
    ])
