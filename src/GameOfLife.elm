module GameOfLife exposing (Flags, Model, Msg(..), RootUrl, State(..), appButton, cellForm, cellForms, init, initialModel, main, playButton, subscriptions, update, view, withBackground)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Cell exposing (Cell, transform, x, y)
import Cell.Collection as Cells exposing (acorn, frothingPuffer, gosperGliderGun, procreate, rpentomino, toList)
import Element as El exposing (centerX, column, el, fill, height, html, layout, padding, paddingXY, px, rgba255, row, spacing, text, width)
import Element.Background as Bg exposing (image)
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Element.Region exposing (heading)
import Html exposing (Html)
import Icons exposing (pause, play, skipBack)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)



-- Model --


type State
    = Running
    | Paused


type alias RootUrl =
    String


type alias Model =
    { assetRoot : RootUrl
    , liveCells : Cells.Collection
    , state : State
    , fps : Int
    }


type alias Flags =
    { assetRoot : String
    }


initialModel : Model
initialModel =
    Model "/" Cells.gosperGliderGun Paused 0


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { initialModel | assetRoot = flags.assetRoot }, Cmd.none )



-- Update --


type Msg
    = Play
    | Pause
    | Reset
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { initialModel | assetRoot = model.assetRoot }, Cmd.none )

        Play ->
            ( { model | state = Running }, Cmd.none )

        Pause ->
            ( { model | state = Paused }, Cmd.none )

        Tick interval ->
            ( { model | liveCells = procreate 100 model.liveCells, fps = round (1000 / interval) }, Cmd.none )



-- View Helpers --


cellForm : Float -> Int -> Int -> Cell -> Svg Msg
cellForm radius scale translate cell =
    let
        scaled =
            Cell.transform (\coord -> coord * scale + translate)
    in
    circle
        [ cx (cell |> scaled |> Cell.x |> String.fromInt)
        , cy (cell |> scaled |> Cell.y |> String.fromInt)
        , r (String.fromFloat radius)
        , SA.fill "#333333"
        ]
        []


cellForms : Cells.Collection -> List (Svg Msg)
cellForms liveCells =
    List.map (cellForm 1.5 3 300) (Cells.toList liveCells)


withBackground : Int -> Int -> List (Svg Msg) -> List (Svg Msg)
withBackground width height cells =
    List.append
        [ rect
            [ SA.x "0"
            , SA.y "0"
            , SA.width <| String.fromInt width
            , SA.height <| String.fromInt height
            , SA.fill "#ffffff"
            , rx "5"
            , ry "5"
            ]
            []
        ]
        cells


appButton : Msg -> Svg Msg -> El.Element Msg
appButton msg icon =
    button
        [ paddingXY 10 5
        , El.width El.fill
        , El.height (px 34)
        , Border.rounded 4
        , Border.width 1
        , Border.color (rgba255 20 20 20 1)
        , Bg.color (rgba255 220 255 255 0.7)
        ]
        { label = html icon, onPress = Just msg }


playButton : State -> El.Element Msg
playButton state =
    case state of
        Running ->
            appButton Pause Icons.pause

        Paused ->
            appButton Play Icons.play



-- View --


view : Model -> Html Msg
view model =
    layout [ Bg.image <| model.assetRoot ++ "images/retro-bg.jpg" ] <|
        column
            [ El.height El.fill, El.width El.fill ]
            [ row
                [ padding 10, El.width El.fill, Bg.color (rgba255 20 20 20 0.7), Font.family [ Font.typeface "Courier", Font.monospace ], Font.color (rgba255 200 200 200 1) ]
                [ el [ centerX, heading 1 ] (El.text "Conway's Game of Life")
                ]
            , column
                [ centerX ]
                [ row
                    [ centerX, padding 5, El.spacing 5 ]
                    [ playButton model.state
                    , appButton Reset Icons.skipBack
                    ]
                , row
                    [ centerX, El.height (px 600) ]
                    [ model.liveCells
                        |> cellForms
                        |> withBackground 600 600
                        |> svg [ viewBox "0 0 600 600", SA.width "600px" ]
                        |> html
                    ]
                , row
                    [ centerX, padding 5, El.spacing 10 ]
                    [ el [ centerX ] (El.text <| "fps:" ++ (model.fps |> String.fromInt))
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
            onAnimationFrameDelta Tick



-- Main --


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
