module Point exposing (Point, PointID, fromString, id, init, map, params, toString)

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
toString (Point (PointID pointID) point) =
    String.join ";"
        [ point.title
        , String.fromInt point.value
        , point.color.value
        , String.fromInt pointID
        ]


fromString : String -> Maybe Point
fromString string =
    case String.split ";" string of
        [ title, stringValue, color, stringID ] ->
            case ( String.toInt stringValue, String.toInt stringID ) of
                ( Just value, Just pointID ) ->
                    Just (Point (PointID pointID) { title = title, value = value, color = Css.hex color })

                _ ->
                    Nothing

        _ ->
            Nothing
