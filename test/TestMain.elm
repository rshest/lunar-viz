import ElmTest exposing (..)

import Tests

testSuite : Test
testSuite =
    suite "All tests"
        [ Tests.model
        , Tests.utils
        ]


main : Program Never
main =
    runSuite testSuite