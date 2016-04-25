module Anim where

import Model exposing (Rover, Action)

type alias RoverAnim =
  { rover : Rover -- rover state
  , spareOffs : (Float, Float) -- spare barrel offset
  , step : Int -- current step in the plan
  , t : Float -- current animation factor [0, 1]
  }


-- interpolates rover state according to action and factor t [0, 1]
interp : Rover -> Action -> Float -> Rover
interp rover action t =
  case action of
    _ -> rover
