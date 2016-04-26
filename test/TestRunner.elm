import Graphics.Element exposing (..)

import ElmTest exposing (elementRunner)

import Tests

main : Element
main = flow down
  [ elementRunner Tests.model
  , elementRunner Tests.utils]
