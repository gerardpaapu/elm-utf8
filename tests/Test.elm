module Main exposing (..)

import ElmTest exposing (..)
import Test.UTF8 as UTF8


tests : Test
tests =
    suite "Tests for UTF-8 encode/decode library"
        [ UTF8.tests
        ]


main : Program Never
main =
    runSuite tests
