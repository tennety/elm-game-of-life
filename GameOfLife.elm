module GameOfLife where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Window
import Set exposing (..)

-- Model --
type alias Cell = (Int, Int)

type alias Model =
  {
    generationCount: Int
  , liveCells: List Cell
  }

init: Int -> List Cell -> Model
init genCount liveCells =
  {
    generationCount = genCount
  , liveCells = liveCells
  }

-- Update --
wrap: Int -> Int
wrap int =
  if | int < -400 -> 400
     | int > 400 -> -400
     | otherwise -> int

neighbors: Cell -> List Cell
neighbors cell =
  [
    (wrap (fst cell - 1), wrap (snd cell + 1))
  , (fst cell, wrap (snd cell + 1))
  , (wrap (fst cell + 1), wrap (snd cell + 1))
  , (wrap (fst cell - 1), snd cell)
  , (wrap (fst cell + 1), snd cell)
  , (wrap (fst cell - 1), wrap (snd cell - 1))
  , (fst cell, wrap (snd cell - 1))
  , (wrap (fst cell + 1), wrap (snd cell - 1))
  ]


procreate: List Cell -> List Cell
procreate cells =
  let
    willSurvive cell =
      let
          family = Set.intersect (Set.fromList (neighbors cell)) (Set.fromList cells)
      in
         List.member (List.length (Set.toList family)) [2,3]
    cellsThatWillSurvive = List.filter willSurvive cells
  -- cellsThatWillBeBorn: dead + 3 live neighbors
  -- cells -- = cellsThatWillSurvive ++ cellsThatWillBeBorn
  in
    List.filter willSurvive cells

update: Time -> Model -> Model
update t model =
  { model | generationCount <- model.generationCount + 1
          , liveCells <- procreate model.liveCells }

-- View --
cellForm: Cell -> Form
cellForm cell =
  filled charcoal (circle 0.5)
  |> move (toFloat (fst cell), toFloat (snd cell))

view: Model -> Element
view model =
  collage 800 800
    ((outlined (solid grey) (rect 800 800)) ::
    (toForm (show model.generationCount) |> move(-395, 390)) ::
    (List.map cellForm model.liveCells))

initialModel: Model
initialModel = init 0 [(0,0), (1,0), (1,2), (3,1), (4,0), (5,0), (6,0)]

gameState =
  Signal.foldp update initialModel (every second)

main =
  Signal.map view gameState
