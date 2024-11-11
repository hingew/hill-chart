module Main exposing (main)

import Browser
import Browser.Navigation as Navigation exposing (Key)
import Css
import Css.Global
import CubicSpline2d
import Direction2d
import Ellipse2d
import Geometry.Svg
import Html.Events.Extra
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events
import Pixels
import Point2d
import Route
import Svg
import Svg.Attributes
import Svg.Styled as SvgStyled
import Svg.Styled.Attributes as SvgStyledAttr
import Svg.Styled.Events
import Tailwind.Utilities as Tw
import Url
import Url.Parser


type alias Model =
    { chart : Chart
    , key : Navigation.Key
    , drag : Drag
    }


type Chart
    = Chart String (List Point)


type alias Point =
    { title : String, value : Int }


type Drag
    = Dragging Int
    | None


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | StartDrag Int
    | StopDrag
    | Move { dx : Int, dy : Int }


init : () -> Url.Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        chart =
            url
                |> parseRoute
                |> chartFromRoute
    in
    ( { chart = chart
      , key = key
      , drag = None
      }
    , Cmd.none
    )


parseRoute : Url.Url -> Route.Route
parseRoute url =
    case Url.Parser.parse Route.parser url of
        Nothing ->
            Route.New

        Just route ->
            route


chartFromRoute : Route.Route -> Chart
chartFromRoute route =
    case route of
        Route.New ->
            Chart "My title"
                [ Point "Test point" 250
                , Point "Test point" 750
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked req ->
            case req of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External _ ->
                    -- We dont have any external urls
                    ( model, Cmd.none )

        UrlChanged url ->
            ( { model
                | chart =
                    url
                        |> parseRoute
                        |> chartFromRoute
              }
            , Cmd.none
            )

        StartDrag index ->
            ( { model | drag = Dragging index }, Cmd.none )

        StopDrag ->
            ( { model | drag = None }, Cmd.none )

        Move { dx } ->
            case model.drag of
                Dragging index ->
                    ( { model | chart = updatePoint dx index model.chart }, Cmd.none )

                None ->
                    ( model, Cmd.none )


updatePoint : Int -> Int -> Chart -> Chart
updatePoint dx pos (Chart title points) =
    Chart title
        (List.indexedMap
            (\index point ->
                if index == pos then
                    { point | value = clamp 0 1000 (point.value + dx) }

                else
                    point
            )
            points
        )


view : Model -> Browser.Document Msg
view model =
    { title = "Hill Chart"
    , body =
        List.map Html.toUnstyled
            [ Css.Global.global Tw.globalStyles
            , Html.div
                [ Attrs.css
                    [ Tw.h_screen
                    , Tw.w_screen
                    , Tw.p_8
                    , Tw.flex
                    , Tw.items_center
                    , Tw.justify_center
                    ]
                , Html.Events.Extra.onMouseMove Move
                , Html.Styled.Events.onMouseUp StopDrag
                ]
                [ viewChart model.chart ]
            ]
    }


viewChart : Chart -> Html Msg
viewChart (Chart title points) =
    Html.div [ Attrs.css [ Tw.flex, Tw.flex_col ] ]
        [ viewTitle title
        , SvgStyled.svg
            [ SvgStyledAttr.width "1000"
            , SvgStyledAttr.height "500"
            , SvgStyledAttr.viewBox "0 0 1000 500"
            ]
            (viewHill :: viewPoints points)
        ]


viewTitle : String -> Html Msg
viewTitle title =
    Html.div [ Attrs.css [ Tw.w_full, Tw.text_center ] ]
        [ Html.h1
            [ Attrs.css [ Tw.font_semibold, Tw.text_3xl ] ]
            [ Html.text title ]
        ]


viewHill : Html Msg
viewHill =
    SvgStyled.g []
        [ SvgStyled.fromUnstyled
            (Geometry.Svg.cubicSpline2d
                [ Svg.Attributes.stroke "blue"
                , Svg.Attributes.fill "none"
                , Svg.Attributes.class "hill-left"
                ]
                hillLeft
            )
        , SvgStyled.fromUnstyled
            (Geometry.Svg.cubicSpline2d
                [ Svg.Attributes.stroke "blue"
                , Svg.Attributes.fill "none"
                , Svg.Attributes.class "hill-right"
                ]
                hillRight
            )

        -- Midline
        , SvgStyled.line
            [ SvgStyledAttr.x1 "500"
            , SvgStyledAttr.y1 "100"
            , SvgStyledAttr.x2 "500"
            , SvgStyledAttr.y2 "440"
            , SvgStyledAttr.stroke "#ccc"
            , SvgStyledAttr.strokeDasharray "5 5"
            , SvgStyledAttr.strokeWidth "2"
            ]
            []

        -- Bottom line
        , SvgStyled.line
            [ SvgStyledAttr.x1 "50"
            , SvgStyledAttr.y1 "400"
            , SvgStyledAttr.x2 "950"
            , SvgStyledAttr.y2 "400"
            , SvgStyledAttr.stroke "#000"
            , SvgStyledAttr.strokeWidth "2"
            ]
            []

        -- Left text
        , SvgStyled.text_
            [ SvgStyledAttr.css
                [ Css.property "text-anchor" "middle"
                , Tw.font_bold
                , Tw.text_lg
                , Tw.select_none
                ]
            , SvgStyledAttr.x "250"
            , SvgStyledAttr.y "425"
            , SvgStyledAttr.fill "#ccc"
            ]
            [ SvgStyled.text "FIGURING THINGS OUT" ]

        -- Left text
        , SvgStyled.text_
            [ SvgStyledAttr.css
                [ Css.property "text-anchor" "middle"
                , Tw.font_bold
                , Tw.text_lg
                , Tw.select_none
                ]
            , SvgStyledAttr.x "750"
            , SvgStyledAttr.y "425"
            , SvgStyledAttr.fill "#ccc"
            ]
            [ SvgStyled.text "MAKING IT HAPPEN" ]
        ]


viewPoints : List Point -> List (Html Msg)
viewPoints =
    List.indexedMap viewPoint


hillRight =
    CubicSpline2d.fromControlPoints
        (Point2d.pixels 500 100)
        (Point2d.pixels 700 100)
        (Point2d.pixels 700 400)
        (Point2d.pixels 900 400)


hillLeft =
    CubicSpline2d.fromControlPoints
        (Point2d.pixels 100 400)
        (Point2d.pixels 300 400)
        (Point2d.pixels 300 100)
        (Point2d.pixels 500 100)


viewPoint : Int -> Point -> Html Msg
viewPoint index point =
    let
        -- Calculate the correct percentage value on the left or right hill curve
        samplePoint =
            if point.value > 500 then
                -- Right side of the hill
                CubicSpline2d.pointOn hillRight ((toFloat point.value / 500) - 1)

            else
                -- left side of the hill
                CubicSpline2d.pointOn hillLeft (toFloat point.value / 500)

        coors =
            Point2d.toRecord Pixels.toFloat samplePoint

        ellipse =
            Ellipse2d.with
                { centerPoint = samplePoint
                , xDirection = Direction2d.degrees 0
                , xRadius = Pixels.float 8
                , yRadius = Pixels.float 8
                }
    in
    SvgStyled.g
        [ Svg.Styled.Events.onMouseDown (StartDrag index)
        ]
        [ SvgStyled.fromUnstyled
            (Geometry.Svg.ellipse2d
                [ Svg.Attributes.fill "orange"
                , Svg.Attributes.style "cursor: move; stroke: #fff; stroke-width: 2px"
                ]
                ellipse
            )
        , SvgStyled.text_
            [ SvgStyledAttr.x (String.fromFloat (coors.x + 12))
            , SvgStyledAttr.y (String.fromFloat (coors.y + 6))
            , SvgStyledAttr.css
                [ Tw.select_none
                , Tw.text_lg
                , Css.property "text-anchor" "start"
                ]
            ]
            [ SvgStyled.text point.title ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
