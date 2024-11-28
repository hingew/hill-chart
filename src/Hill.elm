module Hill exposing
    ( Hill
    , Msg
    , addPoint
    , fromPoints
    , points
    , removePoint
    , subscriptions
    , update
    , updatePoint
    , view
    )

import Browser.Events
import Css
import CubicSpline2d
import Dict
import Direction2d
import Ellipse2d
import Geometry.Svg
import Hill.Confetti
import Hill.Point exposing (Point, PointID)
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Particle.System
import Pixels
import Point2d
import Svg.Attributes
import Svg.Styled as Svg
import Svg.Styled.Attributes as SAttrs
import Tailwind.Utilities as Tw
import Vector2d


type Hill
    = Hill Drag Hill.Confetti.Model (List Point)


type Drag
    = None
    | Dragging PointID Int


hillRight : CubicSpline2d.CubicSpline2d Pixels.Pixels coords
hillRight =
    CubicSpline2d.fromControlPoints
        (Point2d.pixels 500 100)
        (Point2d.pixels 700 100)
        (Point2d.pixels 700 400)
        (Point2d.pixels 900 400)


hillLeft : CubicSpline2d.CubicSpline2d Pixels.Pixels coords
hillLeft =
    CubicSpline2d.fromControlPoints
        (Point2d.pixels 100 400)
        (Point2d.pixels 300 400)
        (Point2d.pixels 300 100)
        (Point2d.pixels 500 100)


points : Hill -> List Point
points (Hill _ _ p) =
    p


addPoint : { title : String, color : Css.Color, value : Int } -> Hill -> Hill
addPoint point (Hill drag confetti ps) =
    Hill drag
        confetti
        (Hill.Point.init
            { title = point.title
            , id = List.length ps
            , color = point.color
            , value = clamp 100 900 point.value
            }
            :: ps
        )


removePoint : PointID -> Hill -> Hill
removePoint id (Hill drag confetti ps) =
    Hill drag confetti (removePointHelp id ps)


removePointHelp : PointID -> List Point -> List Point
removePointHelp id ps =
    case ps of
        [] ->
            []

        x :: xs ->
            if Hill.Point.id x == id then
                xs

            else
                x :: removePointHelp id xs


type Msg
    = StartDrag PointID Int
    | StopDrag
    | Move Int
    | ConfettiMsg Hill.Confetti.Msg


fromPoints : List Point -> Hill
fromPoints ps =
    Hill None Hill.Confetti.init ps


update : Msg -> Hill -> Hill
update msg (Hill drag confetti ps) =
    case msg of
        StartDrag id x ->
            Hill (Dragging id x) confetti ps

        StopDrag ->
            case drag of
                Dragging pointID _ ->
                    if shouldCelebrate pointID ps then
                        Hill None (Hill.Confetti.burst 500 500 confetti) ps

                    else
                        Hill None confetti ps

                _ ->
                    Hill None confetti ps

        Move endX ->
            case drag of
                Dragging pointID startX ->
                    updatePoint
                        (Hill.Point.map
                            (\params ->
                                { params
                                    | value =
                                        clamp 100 900 (params.value + endX - startX)
                                }
                            )
                        )
                        pointID
                        (Hill (Dragging pointID endX) confetti ps)

                None ->
                    Hill None confetti ps

        ConfettiMsg confettiMsg ->
            Hill drag (Hill.Confetti.update confettiMsg confetti) ps


view : Hill -> Html Msg
view (Hill _ confetti ps) =
    Svg.svg
        [ SAttrs.width "1000"
        , SAttrs.height "500"
        , SAttrs.viewBox "0 0 1000 500"
        ]
        (Hill.Confetti.view confetti :: viewHill :: viewPoints ps)


viewHill : Svg.Svg Msg
viewHill =
    Svg.g []
        [ Svg.fromUnstyled
            (Geometry.Svg.cubicSpline2d
                [ Svg.Attributes.stroke "#ccc"
                , Svg.Attributes.fill "none"
                , Svg.Attributes.class "hill-left"
                ]
                hillLeft
            )
        , Svg.fromUnstyled
            (Geometry.Svg.cubicSpline2d
                [ Svg.Attributes.stroke "#ccc"
                , Svg.Attributes.fill "none"
                , Svg.Attributes.class "hill-right"
                ]
                hillRight
            )

        -- Midline
        , Svg.line
            [ SAttrs.x1 "500"
            , SAttrs.y1 "100"
            , SAttrs.x2 "500"
            , SAttrs.y2 "440"
            , SAttrs.stroke "#ccc"
            , SAttrs.strokeDasharray "5 5"
            , SAttrs.strokeWidth "2"
            ]
            []

        -- Bottom line
        , Svg.line
            [ SAttrs.x1 "50"
            , SAttrs.y1 "400"
            , SAttrs.x2 "950"
            , SAttrs.y2 "400"
            , SAttrs.stroke "#ccc"
            , SAttrs.strokeWidth "2"
            ]
            []

        -- FIGURING THINGS OUT
        , Svg.text_
            [ SAttrs.css
                [ Css.property "text-anchor" "middle"
                , Tw.font_bold
                , Tw.text_lg
                , Tw.select_none
                ]
            , SAttrs.x "350"
            , SAttrs.y "425"
            , SAttrs.fill "#ccc"
            ]
            [ Svg.text "FIGURING THINGS OUT" ]

        -- MAKING IT HAPPEN
        , Svg.text_
            [ SAttrs.css
                [ Css.property "text-anchor" "middle"
                , Tw.font_bold
                , Tw.text_lg
                , Tw.select_none
                ]
            , SAttrs.x "650"
            , SAttrs.y "425"
            , SAttrs.fill "#ccc"
            ]
            [ Svg.text "MAKING IT HAPPEN" ]
        ]


{-| Handle mouse subscriptions used for dragging
-}
subscriptions : Hill -> Sub Msg
subscriptions (Hill drag confetti _) =
    case drag of
        None ->
            Sub.map ConfettiMsg (Hill.Confetti.subscriptions confetti)

        Dragging _ _ ->
            Sub.batch
                [ Browser.Events.onMouseMove <| Decode.map Move positionDecoder
                , Browser.Events.onMouseUp <| Decode.succeed StopDrag
                , Sub.map ConfettiMsg (Hill.Confetti.subscriptions confetti)
                ]


dragStartEvent : PointID -> Html.Attribute Msg
dragStartEvent id =
    Events.custom "mousedown"
        (Decode.map
            (\x ->
                { message = StartDrag id x
                , stopPropagation = True
                , preventDefault = True
                }
            )
            positionDecoder
        )


positionDecoder : Decoder Int
positionDecoder =
    Decode.field "pageX" Decode.float |> Decode.map truncate



-- Point


updatePoint : (Point -> Point) -> PointID -> Hill -> Hill
updatePoint fn id (Hill drag confetti ps) =
    Hill drag
        confetti
        (List.map
            (\point ->
                if Hill.Point.id point == id then
                    fn point

                else
                    point
            )
            ps
        )


pointPosition : Int -> Point2d.Point2d Pixels.Pixels coords
pointPosition value =
    let
        normalized : Int
        normalized =
            value - 100
    in
    if normalized > 400 then
        -- Right side of the hill
        CubicSpline2d.pointOn hillRight ((toFloat normalized / 400) - 1)

    else
        -- left side of the hill
        CubicSpline2d.pointOn hillLeft (toFloat normalized / 400)


shouldCelebrate : PointID -> List Point -> Bool
shouldCelebrate pointID ps =
    case ps of
        [] ->
            False

        x :: xs ->
            if Hill.Point.id x == pointID && (Hill.Point.params x).value == 900 then
                True

            else
                shouldCelebrate pointID xs


viewPoints : List Point -> List (Svg.Svg Msg)
viewPoints ps =
    List.foldl
        (\point acc ->
            let
                { value } =
                    Hill.Point.params point

                cluster : Int
                cluster =
                    value - modBy 25 value
            in
            case Dict.get cluster acc of
                Just pts ->
                    Dict.insert cluster (point :: pts) acc

                Nothing ->
                    Dict.insert cluster [ point ] acc
        )
        Dict.empty
        ps
        |> Dict.values
        |> List.concatMap viewGroupedPoints


viewGroupedPoints : List Point -> List (Svg.Svg Msg)
viewGroupedPoints =
    List.indexedMap viewPoint


viewPoint : Int -> Point -> Svg.Svg Msg
viewPoint stackLevel point =
    let
        params : { title : String, value : Int, color : Css.Color }
        params =
            Hill.Point.params point

        pointPos : Point2d.Point2d Pixels.Pixels coords
        pointPos =
            params.value
                |> pointPosition
                |> Point2d.translateBy (Vector2d.pixels 0 (negate (toFloat stackLevel) * 18))

        coords : { x : Float, y : Float }
        coords =
            pointPos
                |> Point2d.toRecord Pixels.toFloat
    in
    Svg.g
        [ dragStartEvent (Hill.Point.id point)
        ]
        [ Svg.fromUnstyled
            (Geometry.Svg.ellipse2d
                [ Svg.Attributes.fill params.color.value
                , Svg.Attributes.style "cursor: move; stroke: #fff; stroke-width: 1px"
                ]
                (Ellipse2d.with
                    { centerPoint = pointPos
                    , xDirection = Direction2d.degrees 0
                    , xRadius = Pixels.float 12
                    , yRadius = Pixels.float 12
                    }
                )
            )
        , Svg.text_
            [ SAttrs.x
                (String.fromFloat
                    (if coords.x < 800 then
                        coords.x + 12

                     else
                        coords.x - 12
                    )
                )
            , SAttrs.y (String.fromFloat (coords.y + 6))
            , SAttrs.css
                [ Tw.select_none
                , Tw.text_lg
                , Css.property "text-anchor"
                    (if coords.x < 800 then
                        "start"

                     else
                        "end"
                    )
                ]
            ]
            [ Svg.text params.title ]
        ]
