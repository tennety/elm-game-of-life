module Cell
    exposing
        ( Cell
        , acorn
        , map
        , procreate
        , rpentomino
        , x
        , y
        )

import Set exposing (..)
import Tuple exposing (..)


type alias Cell =
    ( Int, Int )


x : Cell -> Int
x cell =
    first cell


y : Cell -> Int
y cell =
    second cell


map : (Int -> Int) -> Cell -> Cell
map f cell =
    ( cell |> mapFirst f |> x
    , cell |> mapSecond f |> y
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
        emptyHood =
            Set.empty

        filledHood =
            Set.foldl (Set.union << neighbors) emptyHood cells
    in
    Set.diff filledHood cells


procreate : Set Cell -> Set Cell
procreate cells =
    let
        family cell =
            Set.intersect (neighbors cell) cells

        willSurvive cell =
            List.member (Set.size (family cell)) [ 2, 3 ]

        willBeBorn cell =
            3 == Set.size (family cell)

        cellsThatWillSurvive =
            Set.filter willSurvive cells

        cellsThatWillBeBorn =
            Set.filter willBeBorn (neighborhood cells)
    in
    Set.union cellsThatWillSurvive cellsThatWillBeBorn


rpentomino =
    Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 1, -1 ), ( 2, 1 ) ]


acorn =
    Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 1, 2 ), ( 3, 1 ), ( 4, 0 ), ( 5, 0 ), ( 6, 0 ) ]
