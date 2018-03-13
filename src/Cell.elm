module Cell exposing (Cell, acorn, map, procreate, rpentomino, signal, x, y)

import Set exposing (Set)
import Tuple


type alias Cell =
    ( Int, Int )


x : Cell -> Int
x =
    Tuple.first


y : Cell -> Int
y =
    Tuple.second


map : (Int -> Int) -> Cell -> Cell
map f cell =
    ( cell |> Tuple.mapFirst f |> x
    , cell |> Tuple.mapSecond f |> y
    )


wrap : Int -> Int
wrap int =
    if int < -100 then
        100
    else if int > 100 then
        -100
    else
        int


neighbors : Cell -> Set Cell
neighbors cell =
    Set.fromList
        [ ( wrap (x cell - 1), wrap (y cell + 1) )
        , ( x cell, wrap (y cell + 1) )
        , ( wrap (x cell + 1), wrap (y cell + 1) )
        , ( wrap (x cell - 1), y cell )
        , ( wrap (x cell + 1), y cell )
        , ( wrap (x cell - 1), wrap (y cell - 1) )
        , ( x cell, wrap (y cell - 1) )
        , ( wrap (x cell + 1), wrap (y cell - 1) )
        ]


neighborhood : Set Cell -> Set Cell
neighborhood cells =
    let
        filledHood =
            Set.foldl (Set.union << neighbors) Set.empty cells
    in
    Set.diff filledHood cells


procreate : Set Cell -> Set Cell
procreate cells =
    let
        familySize cell =
            Set.size <| Set.intersect (neighbors cell) cells

        survivalRule cell =
            List.member (familySize cell) [ 2, 3 ]

        birthRule cell =
            3 == familySize cell

        willSurvive =
            Set.filter survivalRule cells

        willBeBorn =
            Set.filter birthRule (neighborhood cells)
    in
    Set.union willSurvive willBeBorn


rpentomino =
    Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 1, -1 ), ( 2, 1 ) ]


acorn =
    Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 1, 2 ), ( 3, 1 ), ( 4, 0 ), ( 5, 0 ), ( 6, 0 ) ]


signal =
    Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( -1, 0 ) ]
