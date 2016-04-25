import Graphics.Element exposing (Element)

import ElmTest exposing (elementRunner)

import Tests

main : Element
main =
    elementRunner Tests.tests
