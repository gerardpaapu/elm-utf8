module UTF8 exposing (toMultiByte, toSingleByte)

{-| Library to encode / decode between multi-byte Unicode characters and UTF-8
multiple single-byte character encoding. Ported from: (c) [Chris
Veness](http://www.movable-type.co.uk).

# UTF-8 to Unicode

@docs toMultiByte

# Unicode to UTF-8

@docs toSingleByte

-}

--LIBS

import Array exposing (Array, fromList, get)
import Bitwise exposing (and, or, shiftLeft, shiftRight)
import Char exposing (KeyCode, fromCode, toCode)
import Regex exposing (HowMany(All), regex, replace)
import String exposing (fromChar, toList)


{-| Encode multi-byte Unicode string into utf-8 multiple single-byte characters
(BMP / basic multilingual plane only).

Chars in range U+0080 - U+07FF are encoded in 2 chars, U+0800 - U+FFFF in 3
chars.

    toMultiByte "Ã¦ Ã¸ Ã¥ Ã±"
        == "æ ø å ñ"
-}
toMultiByte : String -> String
toMultiByte str =
    let
        three =
            -- 3-byte chars
            "[\\u00e0-\\u00ef][\\u0080-\\u00bf][\\u0080-\\u00bf]"

        two =
            -- 2-byte chars
            "[\\u00c0-\\u00df][\\u0080-\\u00bf]"
    in
        str
            |> escape three threeSingleToMulti
            |> escape two twoSingleToMulti


{-| Decode utf-8 encoded string back into multi-byte Unicode characters.

    toSingleByte "æ ø å ñ"
        == "Ã¦ Ã¸ Ã¥ Ã±"
-}
toSingleByte : String -> String
toSingleByte str =
    let
        two =
            -- U+0080 - U+07FF => 2 bytes 110yyyyy, 10zzzzzz
            "[\\u0080-\\u07ff]"

        three =
            -- U+0800 - U+FFFF => 3 bytes 1110xxxx, 10yyyyyy, 10zzzzzz
            "[\\u0800-\\uffff]"
    in
        str
            |> unescape two twoMultiToSingle
            |> unescape three threeMultiToSingle



-- HELPERS


escape : String -> (String -> String) -> String -> String
escape pattern replacement str =
    replace All
        (regex pattern)
        (\{ match } -> replacement match)
        str


unescape : String -> (Char -> String) -> String -> String
unescape pattern replacement str =
    replace All
        (regex pattern)
        (\{ match } ->
            match
                |> String.toList
                |> List.map (replacement)
                |> List.foldl (\c a -> a ++ c) ""
        )
        str


stringify : KeyCode -> String
stringify =
    fromCode >> fromChar


(.&) : Int -> Int -> Int
(.&) =
    Bitwise.and


(.|) : Int -> Int -> Int
(.|) =
    Bitwise.or


(.<<) : Int -> Int -> Int
(.<<) =
    Bitwise.shiftLeft


(.>>) : Int -> Int -> Int
(.>>) =
    Bitwise.shiftRight


threeSingleToMulti : String -> String
threeSingleToMulti three =
    let
        xs =
            strToCharArray three
    in
        ((((getKeyCode 0 xs) .& 0x0F) .<< 12)
            .| (((getKeyCode 1 xs) .& 0x3F) .<< 6)
            .| ((getKeyCode 2 xs) .& 0x3F)
        )
            |> stringify


twoSingleToMulti : String -> String
twoSingleToMulti two =
    let
        xs =
            strToCharArray two
    in
        ((((getKeyCode 0 xs) .& 0x1F) .<< 6)
            .| ((getKeyCode 1 xs) .& 0x3F)
        )
            |> stringify


strToCharArray : String -> Array Char
strToCharArray =
    String.toList >> Array.fromList


getKeyCode : Int -> Array Char -> KeyCode
getKeyCode i xs =
    case Array.get i xs of
        Just c ->
            c |> toCode

        Nothing ->
            0


twoMultiToSingle : Char -> String
twoMultiToSingle c =
    let
        cc =
            c |> toCode
    in
        ((0xC0 .| (cc .>> 6)) |> stringify)
            ++ ((0x80 .| (cc .& 0x3F)) |> stringify)


threeMultiToSingle : Char -> String
threeMultiToSingle c =
    let
        cc =
            c |> toCode
    in
        (0xE0 .| (cc .>> 12) |> stringify)
            ++ ((0x80 .| (cc .>> 6)) .& 0x3F |> stringify)
            ++ ((0x80 .| cc) .& 0x3F |> stringify)
