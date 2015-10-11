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
  if | int < -100 -> 100
     | int > 100 -> -100
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

neighborhood: List Cell -> List Cell
neighborhood cells =
  let
    emptyHood = Set.fromList []
    filledHood = List.foldl (Set.union << Set.fromList << neighbors) emptyHood cells
  in
    Set.toList (Set.diff filledHood (Set.fromList cells))

procreate: List Cell -> List Cell
procreate cells =
  let
    family cell =
      Set.intersect (Set.fromList (neighbors cell)) (Set.fromList cells)

    willSurvive cell =
      List.member (List.length (Set.toList (family cell))) [2,3]

    willBeBorn cell =
      3 == List.length (Set.toList (family cell))

    cellsThatWillSurvive = List.filter willSurvive cells
    cellsThatWillBeBorn = List.filter willBeBorn (neighborhood cells)
  in
    cellsThatWillSurvive ++ cellsThatWillBeBorn

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
    (List.map cellForm model.liveCells))

initialModel: Model
initialModel = init 0 [(0,0), (1,0), (1,2), (3,1), (4,0), (5,0), (6,0)]

gameState =
  Signal.foldp update initialModel (fps 35)

main =
  Signal.map view gameState
