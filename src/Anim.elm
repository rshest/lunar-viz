module Anim where

import Constants exposing (..)
import Model exposing (..)


type Status =
    InProgress
  | Stuck
  | Done


type alias RoverAnim =
  { rover : Rover -- rover state
  , spareOffs : (Float, Float) -- spare barrel offset
  , step : Int -- current step in the plan
  , t : Float -- current animation factor [0, 1]
  , status : Status -- current animation status
  , route : List Action -- list of actions to perform
  }


--  initial animation state
init : RoverAnim
init =
  { rover = Model.init
  , spareOffs = spareOffs
  , step = 0
  , t = 0
  , status = InProgress
  , route = Model.planRoute
  }


--  returns current action for the animation
curAction : RoverAnim -> Maybe Action
curAction anim =
  anim.route |> List.drop anim.step |> List.head



-- interpolates rover state according to action and factor t [0, 1]
interp : RoverAnim -> RoverAnim
interp anim  =
  let interpEval =
    \a ->
      case Model.evalAction (Just anim.rover) a of
        Nothing -> anim
        Just rover -> {anim | rover = rover}
  in
    case curAction anim of
      Just (Move n) -> interpEval (Move (anim.t*n))
      Just (Load n) -> interpEval (Load (anim.t*n))
      Just (Fill n) -> interpEval (Fill (anim.t*n))
      Just (Pick n) -> dumpInterp anim (1 - anim.t)
      Just (Dump) -> dumpInterp anim anim.t
      Nothing -> anim

-- interpolates dump animation (pick is reverse)
dumpInterp : RoverAnim -> Float -> RoverAnim
dumpInterp anim t =
  anim


-- returns animation durations for different actions, seconds
duration : Action -> Float
duration action =
  case action of
    Move n -> abs(n)*moveAnimSpeed
    Load n -> n*loadAnimSpeed
    Fill n -> n*loadAnimSpeed
    Pick n -> dumpAnimSpeed
    Dump   -> dumpAnimSpeed


-- advances scene animation according to the time delta
advance : RoverAnim -> Float -> RoverAnim
advance anim dt =
  case curAction anim of
    Nothing -> {anim | status = Done}
    Just action ->
      let t = anim.t + dt/(duration action) in
        if t < 1 then
          {anim | t = t}
        else
          case Model.evalAction (Just anim.rover) action of
            Nothing -> {anim | status = Stuck}
            Just rover -> advance {anim | step = anim.step + 1,
              t = t - 1, rover = rover} 0
