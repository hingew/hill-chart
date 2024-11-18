module Route exposing (Route(..), parser, toString)

import Hill exposing (Hill)
import Hill.Point
import Url.Builder
import Url.Parser as Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type Route
    = Show String Hill
    | Edit String Hill


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Show (Parser.top <?> titleQuery <?> pointsQuery)
        , Parser.map Edit (Parser.s "edit" <?> titleQuery <?> pointsQuery)
        ]


pointsQuery : Query.Parser Hill
pointsQuery =
    Query.custom "point" (List.filterMap Hill.Point.fromString)
        |> Query.map Hill.fromPoints


titleQuery : Query.Parser String
titleQuery =
    Query.string "title"
        |> Query.map (Maybe.withDefault "")


toString : Route -> String
toString route =
    case route of
        Show title hill ->
            Url.Builder.absolute []
                (Url.Builder.string "title" title
                    :: List.map (Hill.Point.toString >> Url.Builder.string "point")
                        (Hill.points hill)
                )

        Edit title hill ->
            Url.Builder.absolute [ "edit" ]
                (Url.Builder.string "title" title
                    :: List.map (Hill.Point.toString >> Url.Builder.string "point")
                        (Hill.points hill)
                )
