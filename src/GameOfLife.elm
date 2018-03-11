module GameOfLife exposing (..)

import Color exposing (..)
import Html exposing (Html, button, div, program, text)
import Html.Events exposing (onClick)
import Set exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import Tuple exposing (..)


-- Model --


type alias Cell =
    ( Int, Int )


type State
    = Running
    | Paused


type alias Model =
    { liveCells : Set.Set Cell
    , state : State
    }


rpentomino =
    Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 1, -1 ), ( 2, 1 ) ]


acorn =
    Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 1, 2 ), ( 3, 1 ), ( 4, 0 ), ( 5, 0 ), ( 6, 0 ) ]


initialModel : ( Model, Cmd Msg )
initialModel =
    ( Model rpentomino Paused, Cmd.none )



-- Update --


type Msg
    = Start
    | Pause
    | Reset
    | Tick Time.Time


wrap : Int -> Int
wrap int =
    if int < -100 then
        100
    else if int > 100 then
        -100
    else
        int


neighbors : Cell -> Set.Set Cell
neighbors cell =
    Set.fromList
        [ ( wrap (first cell - 1), wrap (second cell + 1) )
        , ( first cell, wrap (second cell + 1) )
        , ( wrap (first cell + 1), wrap (second cell + 1) )
        , ( wrap (first cell - 1), second cell )
        , ( wrap (first cell + 1), second cell )
        , ( wrap (first cell - 1), wrap (second cell - 1) )
        , ( first cell, wrap (second cell - 1) )
        , ( wrap (first cell + 1), wrap (second cell - 1) )
        ]


neighborhood : Set.Set Cell -> Set.Set Cell
neighborhood cells =
    let
        emptyHood =
            Set.fromList []

        filledHood =
            Set.foldl (Set.union << neighbors) emptyHood cells
    in
    Set.diff filledHood cells


procreate : Set.Set Cell -> Set.Set Cell
procreate cells =
    let
        family cell =
            Set.intersect (neighbors cell) cells

        willSurvive cell =
            List.member (List.length (Set.toList (family cell))) [ 2, 3 ]

        willBeBorn cell =
            3 == List.length (Set.toList (family cell))

        cellsThatWillSurvive =
            Set.filter willSurvive cells

        cellsThatWillBeBorn =
            Set.filter willBeBorn (neighborhood cells)
    in
    Set.union cellsThatWillSurvive cellsThatWillBeBorn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | state = Running }, Cmd.none )

        Tick time ->
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
    circle
        [ cx (cell |> first |> toFloat |> (\a -> a * 3 + 300) |> toString)
        , cy (cell |> second |> toFloat |> (\a -> a * 3 + 300) |> toString)
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
                    [ x "0"
                    , y "0"
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
