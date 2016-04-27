module View where

import Utils exposing (..)
import Constants exposing (..)
import Model exposing (Action)
import Anim exposing (RoverAnim)

import Color exposing (..)
import Transform2D exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


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
    [ rect (w - 4) hf |> filled orange |> moveY ((hf - h)*0.5 + 2)
    , img "barrel" w h |> toForm
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
      shake = sin(pos*shakeSpeed)
      soffs = (fst anim.spareOffs, snd anim.spareOffs - shake)
  in [
      barrel (fst tankOffs, snd tankOffs + shake) fuel
    , img "rover" w h |> toForm |>  moveY (hoffs + shake)
    , wheel wheelOffsL (wheelm + (fst wheelPhase))
    , wheel wheelOffsR (wheelm + (snd wheelPhase))
    , if spare >= 0 then barrel soffs spare else group []
    ] |> groupTransform (scaleX -dir) |> moveRot (0, moonRad) (pos*2*pi)


--  draws a tracking path from the base, corresponding to the current rover location
roverPath : Float -> Form
roverPath pos =
  let n = pos*pathGranularity
      nn = round n
      range = if pos > 0 then ([0..nn] |> List.reverse) else [nn..0]
      i2pt = \i -> let ang = -(toFloat i)*2*pi*pos/n in
        (pathRad*sin(ang), pathRad*cos(ang))
      dottedLine = dashed blue
  in
    (List.map i2pt range) |> path |> traced {dottedLine | width = 3}


-- html element for a single action text represenation
actionElem : Float -> Action -> Html
actionElem opacity action =
  let
    textNum = \t n -> t ++ toString(n)
    (cl, txt) =
      case action of
        Model.Move n -> ("act_move",
          textNum (if n < 0 then "cw " else "ccw ") (abs n))
        Model.Load n -> ("act_load", textNum "load " n)
        Model.Fill n -> ("act_fill", textNum "fill " n)
        Model.Pick n -> ("act_pick", textNum "pick " n)
        Model.Dump -> ("act_dump", "dump")
        Model.Stock n -> ("act_stock",
          (if isClose n 1 then "stock" else (textNum "stock " n)))
  in
    span [class cl, style [("opacity", (toString opacity))]] [Html.text txt]

-- full scene display
scene : RoverAnim -> (Int, Int) -> Html
scene anim (w, h) =
  let (mw, mh) = moonExt
      animInt = Anim.interp anim
      rover = animInt.rover
      steps = List.take anim.step anim.route
      actionElems = (List.map (actionElem 1) steps)
      actionLastElem =
        case Anim.curAction anim of
          Nothing -> []
          Just action -> [actionElem anim.t action]
  in
  div []
  [ fromElement (collage mw mh
    [
      img "moon" mw mh |> toForm
    , roverPath rover.pos
    , vehicle animInt
    , group (List.map barrel_ground rover.barrels)
    ])
  , div [class "fuel_pane"]
    [
      span [class "fuel_caption"] [Html.text "Fuel Used:"],
      span [class "fuel_count"] [Html.text (Model.totalFuel steps |> round |> toString)]
    ]
  , div [class "route_pane"] (actionElems ++ actionLastElem)
  ]
