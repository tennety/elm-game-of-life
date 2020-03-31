module Cell exposing (Cell, acorn, frothingPuffer, gosperGliderGun, map, procreate, rpentomino, signal, x, y)

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
    "bob$2ob$b2o!"


acorn =
    "2o2b3o$3bo3b$bo5b!"


signal =
    "3o!"


gosperGliderGun =
    "24bo$22bobo$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o$2o8bo3bob2o4bobo$10bo5bo7bo$11bo3bo$12b2o!"


frothingPuffer =
    "7bo17bo$6b3o15b3o$5b2o4b3o5b3o4b2o$3b2obo2b3o2bo3bo2b3o2bob2o$4bobo2bobo3bobo3bobo2bobo$b2obobobobo4bobo4bobobobob2o$b2o3bobo4bo5bo4bobo3b2o$b3obo3bo4bobobo4bo3bob3o$2o9b2obobobob2o9b2o$12bo7bo$9b2obo7bob2o$10bo11bo$7b2obo11bob2o$7b2o15b2o$7bobobob3ob3obobobo$6b2o3bo3bobo3bo3b2o$6bo2bo3bobobobo3bo2bo$9b2o4bobo4b2o$5b2o4bo3bobo3bo4b2o$9bob2obo3bob2obo$10bobobobobobobo$12bo2bobo2bo$11bobo5bobo!"
