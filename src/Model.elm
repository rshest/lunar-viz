module Model where

fuelConsumption = 5

type Action =
    Move Float  -- move n units (backward if negative)
  | Load Float  -- load n units from the nearby tank
  | Fill Float  -- fill n units from the spare tank
  | Wait Float  -- wait n seconds
  | Pick Float  -- pick a spare tank with n units of fuel
  | Dump        -- dump the spare tank on the ground
