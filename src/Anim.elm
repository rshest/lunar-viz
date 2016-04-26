module Anim where

import Utils
import Constants exposing (..)
import Model exposing (..)

type alias RoverAnim =
  { rover : Rover -- rover state
  , spareOffs : (Float, Float) -- spare barrel offset
  , step : Int -- current step in the plan
  , t : Float -- current animation factor [0, 1]
  }


-- interpolates rover state according to action and factor t [0, 1]
interp : RoverAnim -> Action -> Float -> Maybe RoverAnim
interp anim action t =
  let interpEval =
    \a -> evalAction (Just anim.rover) a
          `Maybe.andThen` \r -> Just {anim | rover = r}
  in
    case action of
      Move n -> interpEval (Move t)
      Load n -> interpEval (Load t)
      Fill n -> interpEval (Fill t)
      Pick n -> Just (dumpInterp anim (1 - t))
      Dump -> Just (dumpInterp anim t)


-- interpolates dump animation (pick is reverse)
dumpInterp : RoverAnim -> Float -> RoverAnim
dumpInterp anim t =
  anim


-- returns animation durations for different actions, seconds
duration : Action -> Float
duration action =
  case action of
    Move n -> n*moveAnimSpeed
    Load n -> n*loadAnimSpeed
    Fill n -> n*loadAnimSpeed
    Pick n -> dumpAnimSpeed
    Dump   -> dumpAnimSpeed
