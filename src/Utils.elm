module Utils where

epsilon : Float
epsilon = 0.0001

--  returns true if two numbers are "close enough"
isClose : Float -> Float -> Bool
isClose a b =
  abs(a - b) < epsilon
