module Html.Events.Extra exposing (onMouseMove)

import Html.Styled
import Html.Styled.Events as Events
import Json.Decode as Decode


onMouseMove : ({ dx : Int, dy : Int } -> msg) -> Html.Styled.Attribute msg
onMouseMove msg =
    Events.on "mousemove"
        (Decode.map2 (\dx dy -> msg { dx = dx, dy = dy })
            (Decode.field "movementX" Decode.int)
            (Decode.field "movementY" Decode.int)
        )
