module Route where

import Model exposing (..)

route = [Move 20,
  Load 100, Pick 100, Move -10, Dump, Move 10,
  Load 100, Pick 100, Move 10, Dump, Move -10,
  Load 100, Pick 100, Move -10, Load 50, Move -10, Dump, Move 10, Load 50, Move 10,
  Load 100, Pick 100, Move 10, Load 50, Move 10, Dump, Move -10, Load 50, Move -10,
  Load 100, Pick 100, Move 10000000]
