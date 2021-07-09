module Hex exposing (toColor)

import Element exposing (Color, rgb255)


validateHex : String -> Bool
validateHex string =
    String.length string
        == 7
        && String.startsWith "#" string
        && String.all Char.isHexDigit (String.dropLeft 1 string)


toInt : Char -> Int
toInt char =
    case Char.toUpper char of
        'A' ->
            10

        'B' ->
            11

        'C' ->
            12

        'D' ->
            13

        'E' ->
            14

        'F' ->
            15

        _ ->
            if Char.isDigit char then
                String.fromChar char
                    |> String.toInt
                    |> Maybe.withDefault 0

            else
                0


toSingle255 : String -> Int
toSingle255 string =
    string |> String.foldl (\current acc -> acc * 16 + toInt current) 0


toColor : String -> Color
toColor string =
    if validateHex string then
        let
            noHash =
                String.dropLeft 1 string
        in
        rgb255
            (String.slice 0 2 noHash |> toSingle255)
            (String.slice 2 4 noHash |> toSingle255)
            (String.slice 4 6 noHash |> toSingle255)

    else
        rgb255 255 0 0
