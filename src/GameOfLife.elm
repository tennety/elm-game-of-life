module GameOfLife exposing (..)

import AppStyles exposing (AppStyles(..), stylesheet)
import Cell exposing (Cell, map, procreate, rpentomino, x, y)
import Color exposing (..)
import Color.Convert exposing (..)
import Element exposing (button, column, el, empty, html, layout, link, row, text, viewport)
import Element.Attributes as EA exposing (center, fill, height, padding, paddingXY, percent, px, spacing, verticalCenter, width)
import Element.Events exposing (onClick)
import Html exposing (Html, program, programWithFlags)
import Icons exposing (pause, play, skipBack)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Time


-- Model --


type State
    = Running
    | Paused


type alias RootUrl =
    String


type alias Model =
    { assetRoot : RootUrl
    , liveCells : Set Cell
    , state : State
    }


type alias Flags =
    { assetRoot : String
    }


initialModel : Model
initialModel =
    Model "/" Cell.rpentomino Paused


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { initialModel | assetRoot = flags.assetRoot }, Cmd.none )



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
            ( Model model.assetRoot Cell.rpentomino Paused, Cmd.none )

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



-- View Helpers --


cellForm : Float -> Int -> Int -> Cell -> Svg Msg
cellForm radius scale translate cell =
    let
        scaled =
            Cell.map (\coord -> coord * scale + translate)
    in
    circle
        [ cx (cell |> scaled |> Cell.x |> toString)
        , cy (cell |> scaled |> Cell.y |> toString)
        , r (toString radius)
        , SA.fill (colorToHex charcoal)
        ]
        []


cellForms : Set Cell -> List (Svg Msg)
cellForms liveCells =
    List.map (cellForm 1.5 3 300) (Set.toList liveCells)


withBackground : Int -> Int -> List (Svg Msg) -> List (Svg Msg)
withBackground width height cells =
    List.append
        [ rect
            [ SA.x "0"
            , SA.y "0"
            , SA.width <| toString width
            , SA.height <| toString height
            , SA.fill <| colorToHex white
            , rx "5"
            , ry "5"
            ]
            []
        ]
        cells


appButton : Msg -> Svg Msg -> Element.Element AppStyles variation Msg
appButton msg icon =
    button Button [ onClick msg, paddingXY 10 5, EA.width (EA.percent 50), EA.height (px 38) ] (html icon)


playButton : State -> Element.Element AppStyles variation Msg
playButton state =
    case state of
        Running ->
            appButton Pause Icons.pause

        Paused ->
            appButton Play Icons.play



-- View --


view : Model -> Html Msg
view model =
    viewport (stylesheet model.assetRoot) <|
        column Body
            [ EA.height <| EA.percent 100 ]
            [ row Header
                [ center, padding 10, EA.width (EA.percent 100) ]
                [ el Title [] (Element.text "Conway's Game of Life")
                ]
            , column Section
                [ center ]
                [ row Toolbar
                    [ padding 5, EA.spacing 5 ]
                    [ playButton model.state
                    , appButton Reset Icons.skipBack
                    ]
                , row Map
                    [ center, EA.height (px 600) ]
                    [ model.liveCells
                        |> cellForms
                        |> withBackground 600 600
                        |> svg [ viewBox "0 0 600 600", SA.width "600px" ]
                        |> html
                    ]
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
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- main =
--     Html.programWithFlags
--         { init = init
--         , view = view
--         , update = update
--         , subscriptions = subscriptions
--         }
