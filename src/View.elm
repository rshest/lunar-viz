module View exposing (..)

import Utils exposing (..)
import Constants exposing (..)
import Model exposing (Action)
import Anim exposing (RoverAnim)
import Msg exposing (..)

import Color exposing (..)
import Transform exposing (..)
import Element exposing (..)
import Collage exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
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
      soffs = (Tuple.first anim.spareOffs, Tuple.second anim.spareOffs - shake)
  in [
      barrel (Tuple.first tankOffs, Tuple.second tankOffs + shake) fuel
    , img "rover" w h |> toForm |>  moveY (hoffs + shake)
    , wheel wheelOffsL (wheelm + (Tuple.first wheelPhase))
    , wheel wheelOffsR (wheelm + (Tuple.second wheelPhase))
    , if spare >= 0 then barrel soffs spare else group []
    ] |> groupTransform (scaleX -dir) |> moveRot (0, moonRad) (pos*2*pi)


--  draws a tracking path from the base, corresponding to the current rover location
roverPath : Float -> Form
roverPath pos =
  let n = pos*pathGranularity
      nn = round n
      range = if pos > 0 then (List.range 0 nn |> List.reverse) else List.range nn 0
      i2pt = \i -> let ang = -(toFloat i)*2*pi*pos/n in
        (pathRad*sin(ang), pathRad*cos(ang))
      dottedLine = dashed blue
  in
    (List.map i2pt range) |> path |> traced {dottedLine | width = 3}


-- html element for a single action text represenation
actionElem : Int -> Float -> Action -> List (Html Msg)
actionElem index opacity action =
  let
    textNum = \t n -> t ++ toString(n)
    (cl, txt) =
      case action of
        Model.Move n -> ("act_move",
          textNum (if n < 0 then "cw " else "ccw ") (abs n))
        Model.Load n -> ("act_load", textNum "load " n)
        Model.Fill n -> ("act_fill", textNum "fill " n)
        Model.Pick n -> ("act_pick", textNum "pick " n)
        Model.Stock n -> ("act_stock",
          (if isClose n 1 then "stock" else (textNum "stock " n)))
        Model.Dump -> ("act_dump", "dump")
    css = style [("opacity", (toString opacity))]
    el = span [class cl, css, onClick (JumpToStep index)] [Html.text txt]
  in
    if cl == "act_stock" then [br [] [], el] else [el]

-- full scene display
view : RoverAnim -> Html Msg
view anim =
  let (mw, mh) = moonExt
      animInt = Anim.interp anim
      rover = animInt.rover
      steps = List.take anim.step anim.route
      indices = List.range 0 (List.length anim.route - 1)
      opacities = indices |> List.map
        (\i -> if i < anim.step then 1
          else if i == anim.step then lerp futureStepOpacity 1 anim.t
          else futureStepOpacity)
      actionElems =
        List.map3 actionElem indices opacities anim.route
        |> List.concat
  in
  div []
  [ toHtml (collage mw mh
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
  , div [class "route_pane"] actionElems
  ]
