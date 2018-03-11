module Css.Colors.MixersTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Css exposing (..)
import Css.Colors.Mixers as Mixers


mock : Color
mock =
    hex "#80e619"


suite : Test
suite =
    describe "Darken"
        [ test "darken 0%" <|
            \_ ->
                Mixers.darken 0 mock
                    |> Expect.equal mock
        , test "darken 20%" <|
            \_ ->
                Mixers.darken 20 mock
                    |> Expect.equal (hex "#67b814")
        , test "darken 100%" <|
            \_ ->
                Mixers.darken 100 mock
                    |> Expect.equal (hex "#000000")
        , test "lighten 0%" <|
            \_ ->
                Mixers.lighten 0 mock
                    |> Expect.equal mock
        , test "lighten 20%" <|
            \_ ->
                Mixers.lighten 20 mock
                    |> Expect.equal (hex "#99eb47")
        , test "lighten 100%" <|
            \_ ->
                Mixers.lighten 100 mock
                    |> Expect.equal (hex "#ffffff")
        ]
