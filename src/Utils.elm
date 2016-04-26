module Utils where

epsilon : Float
epsilon = 0.0001

--  returns true if two numbers are "close enough"
isClose : Float -> Float -> Bool
isClose a b =
  abs(a - b) < epsilon


--  returns equation of parabola (ax^2 + bx + c = 0) through three points
parabola3pt : (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float, Float)
parabola3pt (x1, y1) (x2, y2) (x3, y3) =
  let d = (x1 - x2)*(x1 - x3)*(x2 - x3)
      a = (x3*(y2 - y1) + x2*(y1 - y3) + x1*(y3 - y2))/d
      b = (x3*x3*(y1 - y2) + x2*x2*(y3 - y1) + x1*x1*(y2 - y3))/d
      c = (x2*x3*(x2 - x3)*y1 + x3*x1*(x3 - x1)*y2 + x1*x2*(x1 - x2)*y3)/d
  in (a, b, c)
