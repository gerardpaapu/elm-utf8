module Test.UTF8 exposing (tests)

import ElmTest exposing (..)
import UTF8 exposing (toMultiByte, toSingleByte)


toMultiByte : String -> String
toMultiByte =
    UTF8.toMultiByte


toSingleByte : String -> String
toSingleByte =
    UTF8.toSingleByte


tests : Test
tests =
    suite "UTF-8 Encoding/Decoding"
        [{- test "foo" <|
            assertEqual foo <|
                sha256sum "foo"
         -}
        ]
