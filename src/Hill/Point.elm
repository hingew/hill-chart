module Hill.Point exposing (Point, PointID, fromString, id, init, map, params, toString)

import Css
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Point
    = Point
        PointID
        { title : String
        , value : Int
        , color : Css.Color
        }


type PointID
    = PointID Int


id : Point -> PointID
id (Point pointID _) =
    pointID


params : Point -> { title : String, value : Int, color : Css.Color }
params (Point _ p) =
    p


map :
    ({ title : String, value : Int, color : Css.Color } -> { title : String, value : Int, color : Css.Color })
    -> Point
    -> Point
map fn (Point pointID pointParams) =
    Point pointID (fn pointParams)


init : { title : String, color : Css.Color, value : Int, id : Int } -> Point
init point =
    Point
        (PointID point.id)
        { title = point.title
        , color = point.color
        , value = point.value
        }


toString : Point -> String
toString point =
    Encode.encode 0 (encode point)


encode : Point -> Encode.Value
encode (Point (PointID pointID) p) =
    Encode.object
        [ ( "i", Encode.int pointID )
        , ( "t", Encode.string p.title )
        , ( "v", Encode.int p.value )
        , ( "c", Encode.string p.color.value )
        ]


fromString : String -> Maybe Point
fromString string =
    string
        |> Decode.decodeString decoder
        |> Result.toMaybe


decoder : Decoder Point
decoder =
    Decode.map4 (\pointID title value color -> Point (PointID pointID) { title = title, color = color, value = value })
        (Decode.field "i" Decode.int)
        (Decode.field "t" Decode.string)
        (Decode.field "v" Decode.int)
        (Decode.field "c" (Decode.map Css.hex Decode.string))
