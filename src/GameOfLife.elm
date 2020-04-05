module GameOfLife exposing (Flags, Model, Msg(..), RootUrl, State(..), appButton, cellForm, cellForms, init, initialModel, main, playButton, subscriptions, update, view, withBackground)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Cell exposing (Cell, transform, x, y)
import Cell.Collection as Cells exposing (acorn, fromRle, frothingPuffer, gosperGliderGun, procreate, rpentomino, toList)
import Element as El exposing (centerX, column, el, fill, height, html, layout, padding, paddingXY, px, rgba255, row, spacing, text, width)
import Element.Background as Bg exposing (image)
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Element.Region exposing (heading)
import Html exposing (Html)
import Icons exposing (externalLink, pause, play, skipBack)
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
    , userPattern : String
    }


type alias Flags =
    { assetRoot : String
    }


initialModel : Model
initialModel =
    Model "/" Cells.frothingPuffer Paused 0 ""


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { initialModel | assetRoot = flags.assetRoot }, Cmd.none )



-- Update --


type Msg
    = Play
    | Pause
    | Reset
    | Tick Float
    | UserLoadedPattern Cells.Collection
    | UserEnteredPattern String
    | UserSubmittedPattern


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

        UserLoadedPattern cells ->
            ( { model | liveCells = cells }, Cmd.none )

        UserEnteredPattern rle ->
            ( { model | userPattern = rle }, Cmd.none )

        UserSubmittedPattern ->
            let
                trimmed =
                    String.lines >> String.join ""
            in
            ( { model | liveCells = fromRle (trimmed model.userPattern) }, Cmd.none )



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


formButton : Msg -> Bool -> El.Element Msg -> El.Element Msg
formButton msg selected label =
    let
        borderOpacity =
            if selected then
                1

            else
                0
    in
    button
        [ paddingXY 10 5
        , El.width El.fill
        , El.height (px 34)
        , Border.rounded 4
        , Border.width 2
        , Border.color (rgba255 20 20 20 borderOpacity)
        , Bg.color (rgba255 220 255 255 0.7)
        ]
        { label = label, onPress = Just msg }



-- View --


view : Model -> Html Msg
view model =
    layout [ Bg.image <| model.assetRoot ++ "images/retro-bg.jpg" ] <|
        column
            [ El.height El.fill, El.width El.fill, centerX, Font.family [ Font.typeface "Courier", Font.monospace ] ]
            [ row
                [ padding 10, El.width El.fill, Bg.color (rgba255 20 20 20 0.7), Font.color (rgba255 200 200 200 1) ]
                [ el [ centerX, heading 1 ] (El.text "Conway's Game of Life")
                ]
            , row
                [ El.width El.fill ]
                [ world model
                , form model
                ]
            ]


world : Model -> El.Element Msg
world model =
    column
        [ padding 50, El.width (El.fillPortion 3) ]
        [ row
            [ El.alignRight, El.height (px 600) ]
            [ model.liveCells
                |> cellForms
                |> withBackground 600 600
                |> svg [ viewBox "0 0 600 600", SA.width "600px" ]
                |> html
            ]
        , row
            [ El.alignRight, padding 5, El.spacing 5 ]
            [ el [ centerX ] (El.text <| "fps:" ++ (model.fps |> String.fromInt))
            , playButton model.state
            , appButton Reset Icons.skipBack
            ]
        ]


form : Model -> El.Element Msg
form model =
    column
        [ centerX, padding 50, El.height El.fill, El.width (El.fillPortion 2) ]
        [ column
            [ El.width (El.fill |> El.maximum 400), Bg.color (rgba255 255 255 255 0.7), padding 20, Border.rounded 10 ]
            [ el [ centerX ] (El.text "Select a form")
            , El.wrappedRow
                [ centerX, padding 5, El.spacing 5 ]
                [ formButton (UserLoadedPattern rpentomino) (model.liveCells == rpentomino) (El.text "R-Pentomino")
                , formButton (UserLoadedPattern acorn) (model.liveCells == acorn) (El.text "Acorn")
                , formButton (UserLoadedPattern gosperGliderGun) (model.liveCells == gosperGliderGun) (El.text "Gosper Glider Gun")
                , formButton (UserLoadedPattern frothingPuffer) (model.liveCells == frothingPuffer) (El.text "Frothing Puffer")
                ]
            , el [ centerX, padding 20 ] (El.text "or")
            , Element.Input.multiline
                [ centerX, padding 10, El.height (El.fill |> El.maximum 200) ]
                { onChange = UserEnteredPattern
                , text = model.userPattern
                , placeholder = Just <| Element.Input.placeholder [] (El.text "bob$2ob$b2o!")
                , label = Element.Input.labelAbove [ centerX ] (El.text "Type or paste RLE below")
                , spellcheck = False
                }
            , row
                [ centerX, padding 5, El.spacing 5 ]
                [ formButton UserSubmittedPattern True (El.text "Submit") ]
            , El.paragraph
                [ El.paddingEach { top = 30, right = 0, bottom = 10, left = 0 } ]
                [ El.text "You can find a list of RLE pattern files "
                , El.newTabLink
                    []
                    { url = "https://copy.sh/life/examples/"
                    , label = El.text "here."
                    }
                , el [ El.height (px 20), Font.color (rgba255 50 50 50 0.8) ] (html externalLink)
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
