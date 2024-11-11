module Route exposing (Route(..), parser)

import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = New


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map New Parser.top
        ]
