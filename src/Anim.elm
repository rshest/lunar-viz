module Anim where

import Model exposing (Rover, Action)

-- interpolates rover state according to action and factor t [0, 1]
interp : Rover -> Action -> Float -> Rover
interp rover action t =
  case action of
    _ -> rover
