module GameOfLife where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Time exposing (..)
import Window
import Set exposing (..)

-- Model --
type alias Cell = (Int, Int)

type alias Model =
  {
    generationCount: Int
  , liveCells: Set.Set Cell
  }

rpentomino = Set.fromList [(0,0), (1,0), (1,1), (1,-1), (2,1)]
acorn = Set.fromList [(0,0), (1,0), (1,2), (3,1), (4,0), (5,0), (6,0)]

initialModel: Model
initialModel =
  Model 0 rpentomino

-- Update --
type Action = Start | Pause | Reset
type Mode   = Step  | Continuous

command: Signal.Mailbox Action
command = Signal.mailbox Reset

mode: Signal.Mailbox Mode
mode = Signal.mailbox Continuous

wrap: Int -> Int
wrap int =
  if int < -100 then
    100
  else if int > 100 then
    100
  else
    int

neighbors: Cell -> Set.Set Cell
neighbors cell =
  Set.fromList [
    (wrap (fst cell - 1), wrap (snd cell + 1))
  , (fst cell, wrap (snd cell + 1))
  , (wrap (fst cell + 1), wrap (snd cell + 1))
  , (wrap (fst cell - 1), snd cell)
  , (wrap (fst cell + 1), snd cell)
  , (wrap (fst cell - 1), wrap (snd cell - 1))
  , (fst cell, wrap (snd cell - 1))
  , (wrap (fst cell + 1), wrap (snd cell - 1))
  ]

neighborhood: Set.Set Cell -> Set.Set Cell
neighborhood cells =
  let
    emptyHood = Set.fromList []
    filledHood = Set.foldl (Set.union << neighbors) emptyHood cells
  in
    Set.diff filledHood cells

procreate: Set.Set Cell -> Set.Set Cell
procreate cells =
  let
    family cell =
      Set.intersect (neighbors cell) cells

    willSurvive cell =
      List.member (List.length (Set.toList (family cell))) [2,3]

    willBeBorn cell =
      3 == List.length (Set.toList (family cell))

    cellsThatWillSurvive = Set.filter willSurvive cells
    cellsThatWillBeBorn = Set.filter willBeBorn (neighborhood cells)
  in
    Set.union cellsThatWillSurvive cellsThatWillBeBorn

update: (Time, Action) -> Model -> Model
update (t, action) model =
  case action of
    Start ->
      { model | generationCount = model.generationCount + 1
              , liveCells = procreate model.liveCells }
    Pause -> model
    Reset -> initialModel

-- View --
cellForm: Cell -> Form
cellForm cell =
  filled charcoal (circle 2)
  |> move (4 * toFloat (fst cell), 4 * toFloat (snd cell))

toolbar: Element
toolbar =
  flow right
    [ button (Signal.message command.address Start) "Start"
    , button (Signal.message command.address Pause) "Pause"
    , button (Signal.message command.address Reset) "Reset"
    , dropDown (Signal.message mode.address) [
        ("Step", Step)
      , ("Continuous", Continuous)
      ]
    ]

view: Model -> Element
view model =
  flow down
  [ toolbar
  , collage 800 800
      ((outlined (solid grey) (rect 800 800)) ::
      (List.map cellForm (Set.toList model.liveCells)))
  ]

-- Main --

input: Signal (Time, Action)
input =
  let
    delta = (fps 15)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta command.signal)

gameState =
  Signal.foldp update initialModel input

main =
  Signal.map view gameState
