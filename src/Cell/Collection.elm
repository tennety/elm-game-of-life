module Cell.Collection exposing (Collection, acorn, fromList, fromRle, frothingPuffer, gosperGliderGun, procreate, rpentomino, signal, toList)

import Cell exposing (Cell, fromTuple, toTuple)
import RleParser as Rle
import Set exposing (Set)


type Collection
    = Collection (Set ( Int, Int ))


fromRle : String -> Collection
fromRle rle =
    rle
        |> Rle.parse
        |> Result.withDefault []
        |> List.map Cell.fromTuple
        |> fromList


fromList : List Cell -> Collection
fromList =
    Collection << Set.fromList << List.map toTuple


toList : Collection -> List Cell
toList (Collection cells) =
    cells
        |> Set.toList
        |> List.map fromTuple


procreate : Int -> Collection -> Collection
procreate limit (Collection cells) =
    let
        familySize cell =
            Set.size <| Set.intersect (neighbors limit cell) cells

        survivalRule cell =
            List.member (familySize cell) [ 2, 3 ]

        birthRule cell =
            3 == familySize cell

        willSurvive =
            Set.filter survivalRule cells

        willBeBorn =
            Set.filter birthRule (neighborhood limit cells)
    in
    Collection (Set.union willSurvive willBeBorn)


rpentomino =
    fromRle "bob$2ob$b2o!"


acorn =
    fromRle "2o2b3o$3bo3b$bo5b!"


signal =
    fromRle "3o!"


gosperGliderGun =
    fromRle "24bo$22bobo$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o$2o8bo3bob2o4bobo$10bo5bo7bo$11bo3bo$12b2o!"


frothingPuffer =
    fromRle "7bo17bo$6b3o15b3o$5b2o4b3o5b3o4b2o$3b2obo2b3o2bo3bo2b3o2bob2o$4bobo2bobo3bobo3bobo2bobo$b2obobobobo4bobo4bobobobob2o$b2o3bobo4bo5bo4bobo3b2o$b3obo3bo4bobobo4bo3bob3o$2o9b2obobobob2o9b2o$12bo7bo$9b2obo7bob2o$10bo11bo$7b2obo11bob2o$7b2o15b2o$7bobobob3ob3obobobo$6b2o3bo3bobo3bo3b2o$6bo2bo3bobobobo3bo2bo$9b2o4bobo4b2o$5b2o4bo3bobo3bo4b2o$9bob2obo3bob2obo$10bobobobobobobo$12bo2bobo2bo$11bobo5bobo!"



--- Internal API ---


wrap : Int -> Int -> Int
wrap limit value =
    if value < negate limit then
        limit

    else if value > limit then
        negate limit

    else
        value


neighbors : Int -> ( Int, Int ) -> Set ( Int, Int )
neighbors limit ( x_, y_ ) =
    let
        wrapAtLimit =
            wrap limit
    in
    Set.fromList
        [ ( wrapAtLimit (x_ - 1), wrapAtLimit (y_ + 1) )
        , ( x_, wrapAtLimit (y_ + 1) )
        , ( wrapAtLimit (x_ + 1), wrapAtLimit (y_ + 1) )
        , ( wrapAtLimit (x_ - 1), y_ )
        , ( wrapAtLimit (x_ + 1), y_ )
        , ( wrapAtLimit (x_ - 1), wrapAtLimit (y_ - 1) )
        , ( x_, wrapAtLimit (y_ - 1) )
        , ( wrapAtLimit (x_ + 1), wrapAtLimit (y_ - 1) )
        ]


neighborhood limit cells =
    let
        filledHood =
            Set.foldl (Set.union << neighbors limit) Set.empty cells
    in
    Set.diff filledHood cells



--- End Internal API ---
