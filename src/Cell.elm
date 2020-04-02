module Cell exposing (Cell, fromTuple, toTuple, transform, x, y)

import Set exposing (Set)
import Tuple


type Cell
    = Cell Int Int


toTuple : Cell -> ( Int, Int )
toTuple (Cell x_ y_) =
    ( x_, y_ )


fromTuple : ( Int, Int ) -> Cell
fromTuple ( x_, y_ ) =
    Cell x_ y_


x : Cell -> Int
x (Cell x_ _) =
    x_


y : Cell -> Int
y (Cell _ y_) =
    y_


transform : (Int -> Int) -> Cell -> Cell
transform f (Cell x_ y_) =
    Cell (f x_) (f y_)
