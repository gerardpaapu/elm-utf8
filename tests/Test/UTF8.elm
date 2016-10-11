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
        [ test "UTF-8 to Unicode" <|
            assertEqual "æ ø å ñ" <|
                toMultiByte "Ã¦ Ã¸ Ã¥ Ã±"
        , test "Unicode to UTF-8" <|
            assertEqual "Ã¦ Ã¸ Ã¥ Ã±" <|
                toSingleByte "æ ø å ñ"
        ]
