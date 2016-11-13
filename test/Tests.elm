module Tests exposing (..)
import ElmTest exposing (..)

import Utils exposing (..)
import Model exposing (..)


evalPath : List Action -> Maybe Rover
evalPath = Model.evalActions (Just {init | fuel = 1, spare = 1})

model : Test
model = suite "Model Eval"
  [
  test "Can't move"
    (let r = evalPath [Move 0.1, Move -0.2] in
      assertEqual (Nothing) (r)),

  test "Move right"
    (let r = evalPath [Move 0.1] in
      assertEqual (Just {pos = 0.1, dir = 1, fuel = 0.5, spare = 1, barrels = []}) (r)),

  test "Move RL"
    (let r = evalPath [Move 0.1, Move -0.05] in
      assertEqual (Just {pos = 0.05, dir = -1, fuel = 0.25, spare = 1, barrels = []}) (r)),

  test "Can't load - tank full"
    (let r = evalPath [Load 0.1] in
      assertEqual (Nothing) (r)),

  test "Can't load - no barrel"
    (let r = evalPath [Move 0.1, Load 0.1] in
      assertEqual (Nothing) (r)),

  test "Load half"
    (let r = evalPath [Move 0.1, Move -0.1, Load 0.75, Move -0.1] in
      assertEqual (Just {pos = -0.1, dir = -1, fuel = 0.25, spare = 1, barrels = []}) (r)),

  test "Can move because fill"
    (let r = evalPath [Move 0.1, Fill 0.5, Move -0.2, Fill 0.5, Move 0.1] in
      assertEqual (Just {pos = 0.0, dir = 1, fuel = 0.0, spare = 0, barrels = []}) (r)),

  test "Can't fill - no space"
    (let r = evalPath [Move 0.1, Fill 1] in
      assertEqual (Nothing) (r)),

  test "Can't fill - no fuel"
    (let r = evalPath [Move 0.1, Fill 0.5, Move 0.1, Fill 0.25, Move 0.1, Fill 0.5] in
      assertEqual (Nothing) (r)),

  test "No pick not at base"
    (let r = evalPath [Move 0.1, Pick 1] in
      assertEqual (Nothing) (r)),

  test "No pick no empty"
    (let r = evalPath [Pick 1] in
      assertEqual (Nothing) (r)),

  test "Dump"
    (let r = evalPath [Move 0.05, Dump, Move -0.05] in
      assertEqual (Just {pos = 0.0, dir = -1, fuel = 0.5, spare = -1, barrels = [(0.05, 1)]}) (r)),

  test "Dump and load"
    (let r = evalPath [Move 0.05, Dump, Move 0.05, Move -0.05, Load 0.5, Move -0.05] in
      assertEqual (Just {pos = 0.0, dir = -1, fuel = 0.5, spare = -1, barrels = [(0.05, 0.5)]}) (r)),

  test "Can't load - no space"
    (let r = evalPath [Move 0.1, Dump, Load 1] in
      assertEqual (Nothing) (r)),

  test "Load all fuel"
    (let r = evalPath [Move 0.1, Dump, Load 0.5, Move 0.1, Move -0.1, Load 0.5] in
      assertEqual (Just {pos = 0.1, dir = -1, fuel = 0.5, spare = -1, barrels = [(0.1, 0)]}) (r)),

  test "Can't load - no fuel"
    (let r = evalPath [Move 0.1, Dump, Load 0.5, Move 0.1, Move -0.1, Load 1] in
      assertEqual (Nothing) (r)),

  test "Stock"
    (let r = evalPath [Move 0.05, Dump, Move -0.05, Stock 1] in
      assertEqual (Just {pos = 0.0, dir = -1, fuel = 1, spare = 1, barrels = [(0.05, 1)]}) (r))
  ]


utils : Test
utils = suite "Utils"
  [
  test "Parabola from 3 points"
    (let (a, b, c) = parabolaFrom3pt (0, -3) (1, 0) (2, 7) in
      assert (isClose a 2 && isClose b 1 && isClose c -3)),

  test "Parabola equation"
    (let y = parabolaPt (2, 1, -3) 2 in
      assert (isClose 7 y)),

  test "Lerp"
    (let res = List.map (lerp 1 2) [0, 0.5, 0.25, 1, -2, -1, 10] in
      assertEqual res [1, 1.5, 1.25,  2, 1, 1, 2])
  ]
