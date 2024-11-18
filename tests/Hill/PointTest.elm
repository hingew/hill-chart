module Hill.PointTest exposing (suite)

import Css
import Expect
import Fuzz
import Hill.Point as Point exposing (Point)
import Test exposing (Test, describe, fuzz)


pointFuzzer : Fuzz.Fuzzer Point
pointFuzzer =
    Fuzz.map4 (\title color value id -> Point.init { title = title, color = color, value = value, id = id })
        Fuzz.string
        colorFuzzer
        Fuzz.int
        Fuzz.int


colorFuzzer : Fuzz.Fuzzer Css.Color
colorFuzzer =
    Fuzz.oneOfValues
        [ Css.hex "#f000000"
        , Css.hex "#0f00000"
        , Css.hex "#00f0000"
        ]


suite : Test
suite =
    describe "Hill.Point"
        [ serialization ]


serialization : Test
serialization =
    fuzz pointFuzzer "serialization roundtrip" <|
        \point ->
            point
                |> Point.toString
                |> Point.fromString
                |> Expect.equal (Just point)
