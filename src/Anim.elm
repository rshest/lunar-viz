module Anim exposing (..)

import Utils
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
  , spareOffs = restSpareOffs
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
    \a -> case Model.evalAction a (Just anim.rover) of
            Nothing -> anim
            Just rover -> {anim | rover = rover}
  in
    case curAction anim of
      Just (Move n) -> interpEval (Move (anim.t*n))
      Just (Load n) -> interpEval (Load (anim.t*n))
      Just (Fill n) -> interpEval (Fill (anim.t*n))
      Just (Pick n) -> dumpInterp anim n (1 - anim.t)
      Just (Dump) -> dumpInterp anim anim.rover.spare anim.t
      Just (Stock n) ->
        let anim1 = interpEval (Load (anim.t*(1 - anim.rover.fuel))) in
         dumpInterp anim1 n (1 - anim.t)
      Nothing -> anim


-- parabola equation for the tank dumping annimation
dumpParabola : (Float, Float, Float)
dumpParabola =
  Utils.parabolaFrom3pt (0, 0) ((fst restSpareOffs)/2, dumpHeight) restSpareOffs


-- interpolates dump animation (pick is reverse)
dumpInterp : RoverAnim -> Float -> Float -> RoverAnim
dumpInterp anim n t =
  let x = (fst restSpareOffs)*(1 - t)
      y =  Utils.parabolaPt dumpParabola x
      rover = anim.rover
  in
    { anim | spareOffs = (x, y), rover = {rover | spare = n } }


-- returns animation durations for different actions, seconds
duration : Action -> Float
duration action =
  case action of
    Move n -> abs(n)*moveAnimSpeed
    Load n -> n*loadAnimSpeed
    Fill n -> n*loadAnimSpeed
    Pick n -> dumpAnimSpeed
    Stock n -> dumpAnimSpeed
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
          case Model.evalAction action (Just anim.rover) of
            Nothing -> {anim | status = Stuck}
            Just rover -> advance {anim | step = anim.step + 1,
              t = t - 1, rover = rover} 0


--  advances animation from the beginning to the given step
jumpToStep : RoverAnim -> Int -> RoverAnim
jumpToStep anim n =
  let route = anim.route |> List.take n
      rover = case Model.evalActions (Just Model.init) route of
        Just r -> r
        Nothing -> anim.rover
  in
    {anim | step = n, t = 0, rover = rover}
