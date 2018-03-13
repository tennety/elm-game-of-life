module GameOfLife exposing (..)

import Cell exposing (Cell, map, procreate, rpentomino, x, y)
import Element exposing (button, column, el, empty, html, layout, link, row, text, viewport)
import Element.Attributes as ElAttrs exposing (center, fill, height, padding, px, spacing, width)
import Element.Events exposing (onClick)
import Html exposing (Html, program)
import Set exposing (Set)
import Styles exposing (GolStyles(..), stylesheet)
import Svg exposing (..)
import Svg.Attributes as SvgAttrs exposing (..)
import Time


-- Model --


type State
    = Running
    | Paused


type alias Model =
    { liveCells : Set Cell
    , state : State
    }


initialModel : ( Model, Cmd Msg )
initialModel =
    ( Model Cell.rpentomino Paused, Cmd.none )



-- Update --


type Msg
    = Play
    | Pause
    | Reset
    | Tick Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            initialModel

        Play ->
            ( { model | state = Running }, Cmd.none )

        Pause ->
            ( { model | state = Paused }, Cmd.none )

        Tick _ ->
            case model.state of
                Running ->
                    ( { model | liveCells = procreate model.liveCells }, Cmd.none )

                Paused ->
                    ( model, Cmd.none )



-- View --


view : Model -> Html Msg
view model =
    let
        cellForm : Cell -> Svg Msg
        cellForm cell =
            let
                scaled =
                    Cell.map (\coord -> coord * 3 + 300)
            in
            circle
                [ cx (cell |> scaled |> Cell.x |> toString)
                , cy (cell |> scaled |> Cell.y |> toString)
                , r "1.5"
                , SvgAttrs.fill "#333"
                ]
                []

        cells : Model -> List (Svg Msg)
        cells model =
            List.map cellForm (Set.toList model.liveCells)

        playButton : Model -> Element.Element GolStyles variation Msg
        playButton model =
            case model.state of
                Running ->
                    button Button [ onClick Pause, padding 10 ] (Element.text "Pause")

                Paused ->
                    button Button [ onClick Play, padding 10 ] (Element.text "Play")
    in
    viewport stylesheet <|
        column Body
            []
            [ el Title [ center ] (Element.text "Game of Life")
            , row Toolbar
                [ center, padding 5, ElAttrs.spacing 5 ]
                [ playButton model
                , button Button [ onClick Reset, padding 10 ] (Element.text "Reset")
                ]
            , row Map
                [ center, ElAttrs.height (px 600) ]
                [ html <|
                    svg [ viewBox "0 0 600 600", SvgAttrs.width "600px" ]
                        (List.append
                            [ rect
                                [ SvgAttrs.x "0"
                                , SvgAttrs.y "0"
                                , SvgAttrs.width "600"
                                , SvgAttrs.height "600"
                                , SvgAttrs.fill "none"
                                , stroke "#ccc"
                                , rx "5"
                                , ry "5"
                                ]
                                []
                            ]
                            (cells model)
                        )
                ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Paused ->
            Sub.none

        Running ->
            Time.every (Time.second / 10) Tick



-- Main --


main =
    Html.program
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
