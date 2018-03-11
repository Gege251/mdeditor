module Css.Colors.Mixers exposing (..)

import Css exposing (..)
import Hex


withPrecedingHash : String -> String
withPrecedingHash str =
    if String.startsWith "#" str then
        str
    else
        String.cons '#' str


darken : Int -> Color -> Color
darken amount color =
    if List.length color.warnings == 0 then
        let
            red =
                color.red - (color.red * amount // 100)

            green =
                color.green - (color.green * amount // 100)

            blue =
                color.blue - (color.blue * amount // 100)

            value =
                toHexColor red green blue
        in
            { color
                | red = red
                , green = green
                , blue = blue
                , value = value
            }
    else
        color


lighten : Int -> Color -> Color
lighten amount color =
    if List.length color.warnings == 0 then
        let
            red =
                color.red + ((255 - color.red) * amount // 100)

            green =
                color.green + ((255 - color.green) * amount // 100)

            blue =
                color.blue + ((255 - color.blue) * amount // 100)

            value =
                toHexColor red green blue
        in
            { color
                | red = red
                , green = green
                , blue = blue
                , value = value
            }
    else
        color


toHexColor : Int -> Int -> Int -> String
toHexColor red green blue =
    [ red, green, blue ]
        |> List.map (Hex.toString >> withLeadingZero 2)
        |> String.concat
        |> String.cons '#'


withLeadingZero : Int -> String -> String
withLeadingZero length str =
    if String.length str < length then
        withLeadingZero length (String.cons '0' str)
    else
        str
