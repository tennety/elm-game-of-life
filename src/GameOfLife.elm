module GameOfLife exposing (..)

import Cell exposing (Cell, map, procreate, rpentomino, x, y)
import Html exposing (Html, button, div, program, text)
import Html.Events exposing (onClick)
import Set exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgAttrs exposing (..)
import Time
import Tuple exposing (..)


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
    = Start
    | Pause
    | Reset
    | Tick Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | state = Running }, Cmd.none )

        Tick _ ->
            case model.state of
                Running ->
                    ( { model | liveCells = procreate model.liveCells }, Cmd.none )

                Paused ->
                    ( model, Cmd.none )

        Pause ->
            ( { model | state = Paused }, Cmd.none )

        Reset ->
            initialModel



-- View --


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
        , fill "#333"
        ]
        []


cells : Model -> List (Svg Msg)
cells model =
    List.map cellForm (Set.toList model.liveCells)


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick Start ] [ Html.text "Start" ]
            , button [ onClick Pause ] [ Html.text "Pause" ]
            , button [ onClick Reset ] [ Html.text "Reset" ]
            ]
        , svg [ viewBox "0 0 600 600", width "600px" ]
            (List.append
                [ rect
                    [ SvgAttrs.x "0"
                    , SvgAttrs.y "0"
                    , width "600"
                    , height "600"
                    , fill "none"
                    , stroke "#ccc"
                    , rx "5"
                    , ry "5"
                    ]
                    []
                ]
                (cells model)
            )
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
