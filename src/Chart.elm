module Hill exposing (Chart(..), Color(..), Point, PointID)


type Hill
    = Hill String (List Point)


type alias Point =
    { title : String
    , value : Float
    , color : Color
    , id : PointID
    }


type PointID
    = PointID Int


type Color
    = Blue
    | Green
    | Orange
    | Red
