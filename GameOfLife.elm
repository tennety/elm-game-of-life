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
  , liveCells: Set.Set Cell
  }

init: Int -> Set.Set Cell -> Model
init genCount liveCells =
  {
    generationCount = genCount
  , liveCells = liveCells
  }

rpentomino = Set.fromList [(0,0), (1,0), (1,1), (1,-1), (2,1)]
acorn = Set.fromList [(0,0), (1,0), (1,2), (3,1), (4,0), (5,0), (6,0)]

-- Update --
wrap: Int -> Int
wrap int =
  if | int < -100 -> 100
     | int > 100 -> -100
     | otherwise -> int

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
  collage 200 200
    ((outlined (solid grey) (rect 200 200)) ::
    (List.map cellForm (Set.toList model.liveCells)))

initialModel: Model
initialModel = init 0 rpentomino

gameState =
  Signal.foldp update initialModel (fps 15)

main =
  Signal.map view gameState
