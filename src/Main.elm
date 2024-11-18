module Main exposing (main)

import Browser
import Browser.Navigation as Navigation exposing (Key)
import Css
import Css.Global
import Hill exposing (Hill)
import Hill.Point exposing (Point, PointID)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events
import Route exposing (Route)
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Url
import Url.Parser


type alias Model =
    { key : Navigation.Key
    , route : Route
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | UpdateTitle String
    | UpdateHill Hill.Msg
    | UpdatePointTitle PointID String
    | UpdatePointColor PointID Css.Color
    | AddPoint
    | RemovePoint PointID
    | Save
    | Edit
    | Cancel


init : () -> Url.Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { route = parseRoute url
      , key = key
      }
    , Cmd.none
    )


parseRoute : Url.Url -> Route.Route
parseRoute url =
    case Url.Parser.parse Route.parser url of
        Nothing ->
            Route.Show "" (Hill.fromPoints [])

        Just route ->
            route


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
            ( { model | route = parseRoute url }, Cmd.none )

        UpdateHill hillMsg ->
            ( { model | route = mapHill (Hill.update hillMsg) model.route }, Cmd.none )

        UpdateTitle title ->
            ( { model | route = setTitle title model.route }, Cmd.none )

        UpdatePointTitle pointID title ->
            ( { model
                | route =
                    mapHill
                        (Hill.updatePoint (Hill.Point.map (\params -> { params | title = title }))
                            pointID
                        )
                        model.route
              }
            , Cmd.none
            )

        UpdatePointColor pointID color ->
            ( { model
                | route =
                    mapHill
                        (Hill.updatePoint (Hill.Point.map (\point -> { point | color = color })) pointID)
                        model.route
              }
            , Cmd.none
            )

        AddPoint ->
            ( { model
                | route =
                    mapHill
                        (Hill.addPoint
                            { title = "Point"
                            , color = Css.hex "#f00"
                            , value = 100
                            }
                        )
                        model.route
              }
            , Cmd.none
            )

        RemovePoint id ->
            ( { model | route = mapHill (Hill.removePoint id) model.route }, Cmd.none )

        Save ->
            case model.route of
                Route.Show _ _ ->
                    ( model, Cmd.none )

                Route.Edit title hill ->
                    ( model, Navigation.pushUrl model.key (Route.toString (Route.Show title hill)) )

        Edit ->
            case model.route of
                Route.Show title hill ->
                    ( model, Navigation.pushUrl model.key (Route.toString (Route.Edit title hill)) )

                Route.Edit _ _ ->
                    ( model, Cmd.none )

        Cancel ->
            case model.route of
                Route.Show _ _ ->
                    ( model, Cmd.none )

                Route.Edit _ _ ->
                    ( model, Navigation.back model.key 1 )


mapHill : (Hill -> Hill) -> Route -> Route
mapHill fn route =
    case route of
        Route.Show _ _ ->
            route

        Route.Edit title hill ->
            Route.Edit title (fn hill)


setTitle : String -> Route -> Route
setTitle title route =
    case route of
        Route.Show _ _ ->
            route

        Route.Edit _ hill ->
            Route.Edit title hill


view : Model -> Browser.Document Msg
view model =
    { title = "Hill Chart"
    , body =
        List.map Html.toUnstyled
            [ Css.Global.global Tw.globalStyles
            , Html.div
                [ Attrs.css
                    [ Tw.h_screen
                    , Tw.mx_auto
                    , Tw.max_w_7xl
                    , Tw.p_8
                    , Tw.flex
                    , Tw.flex_col
                    , Tw.items_center
                    ]
                ]
                (case model.route of
                    Route.Show title hill ->
                        [ viewChart False title hill ]

                    Route.Edit title hill ->
                        [ viewChart True title hill
                        , viewForm hill
                        ]
                )
            ]
    }


viewChart : Bool -> String -> Hill -> Html Msg
viewChart editing title hill =
    Html.div
        [ Attrs.css
            [ Tw.flex
            , Tw.flex_col
            , Tw.justify_center
            , Tw.items_center
            , Tw.w_full
            ]
        ]
        [ viewTitle editing title
        , Html.map UpdateHill (Hill.view hill)
        ]


viewTitle : Bool -> String -> Html Msg
viewTitle editing title =
    Html.div [ Attrs.css [ Tw.flex, Tw.flex_col, Tw.w_full, Tw.text_center, Tw.gap_2 ] ]
        [ Html.div [ Attrs.css [ Tw.flex, Tw.flex_row, Tw.justify_between, Tw.items_center, Tw.w_full ] ]
            (if editing then
                [ viewButton Cancel "Cancel"
                , viewButton Save "Save"
                ]

             else
                [ viewButton Edit "Edit"
                , Html.div [ Attrs.css [] ] []
                ]
            )
        , if editing then
            Html.input
                [ Attrs.type_ "string"
                , Html.Styled.Events.onInput UpdateTitle
                , Attrs.value title
                , Attrs.css [ Tw.font_semibold, Tw.text_3xl, Tw.text_center ]
                ]
                []

          else
            Html.h1
                [ Attrs.css [ Tw.font_semibold, Tw.text_3xl ] ]
                [ Html.text title ]
        ]


viewButton : Msg -> String -> Html Msg
viewButton msg label =
    Html.button
        [ Html.Styled.Events.onClick msg
        , Attrs.css
            [ Tw.border_2
            , Tw.relative
            , Tw.rounded_md
            , Tw.cursor_pointer
            , Tw.text_lg
            , Tw.px_4
            , Tw.py_1
            ]
        ]
        [ Html.text label ]


viewForm : Hill -> Html Msg
viewForm hill =
    Html.div [ Attrs.css [ Tw.flex, Tw.flex_col, Tw.gap_4, Tw.w_full, Tw.max_w_2xl ] ]
        (Html.div [ Attrs.css [ Tw.flex, Tw.justify_end ] ] [ viewButton AddPoint "Add point" ]
            :: List.map viewPointForm (Hill.points hill)
        )


viewPointForm : Point -> Html Msg
viewPointForm point =
    let
        params : { title : String, value : Int, color : Css.Color }
        params =
            Hill.Point.params point

        id : PointID
        id =
            Hill.Point.id point
    in
    Html.div [ Attrs.css [ Tw.flex, Tw.w_full, Tw.flex_row, Tw.gap_4 ] ]
        [ Html.input
            [ Attrs.type_ "string"
            , Html.Styled.Events.onInput (UpdatePointTitle id)
            , Attrs.value params.title
            , Attrs.css [ Tw.border, Tw.px_4, Tw.py_1, Tw.w_full, Tw.rounded_md ]
            ]
            []
        , viewColorInput id params.color
        , viewButton (RemovePoint id) "Remove"
        ]


viewColorInput : PointID -> Css.Color -> Html Msg
viewColorInput pointID value =
    let
        colorButton : Css.Color -> Css.Color -> Html Msg
        colorButton c activeColor =
            Html.button
                [ Html.Styled.Events.onClick (UpdatePointColor pointID c)
                , Attrs.css
                    [ Tw.rounded_full
                    , Tw.border_2
                    , if c == activeColor then
                        Tw.border_color Theme.gray_500

                      else
                        Tw.border_color Theme.gray_100
                    , Tw.h_8
                    , Tw.w_8
                    , Css.backgroundColor c
                    ]
                ]
                []
    in
    Html.div [ Attrs.css [ Tw.w_full, Tw.flex, Tw.flex_row, Tw.gap_2 ] ]
        [ colorButton (Css.hex "#f00") value
        , colorButton (Css.hex "#0f0") value
        , colorButton (Css.hex "#0ff") value
        , colorButton (Css.hex "#00f") value
        , colorButton (Css.hex "#f0f") value
        , Html.input
            [ Attrs.type_ "color"
            , Html.Styled.Events.onInput (\colorString -> UpdatePointColor pointID (Css.hex colorString))
            , Attrs.css
                [ Css.pseudoElement "-webkit-color-swatch" [ Tw.rounded_full ]
                , Css.pseudoElement "-webkit-color-swatch-wrapper" [ Tw.p_0 ]
                , Css.pseudoElement "-moz-color-swatch" [ Tw.rounded_full ]
                , Tw.rounded_full
                , Tw.border_2
                , Tw.h_8
                , Tw.w_8
                , Tw.cursor_pointer
                ]
            ]
            []
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.route of
        Route.Show _ _ ->
            Sub.none

        Route.Edit _ hill ->
            Sub.map UpdateHill (Hill.subscriptions hill)


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
