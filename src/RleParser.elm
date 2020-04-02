module RleParser exposing (parse)

import Parser exposing (..)


type alias GridState =
    { x : Int
    , y : Int
    , cellList : List ( Int, Int )
    }


type CellState
    = Alive
    | Dead


init =
    { x = 0
    , y = 0
    , cellList = []
    }


parse : String -> Result (List DeadEnd) (List ( Int, Int ))
parse =
    run cells


cells : Parser (List ( Int, Int ))
cells =
    loop init cellHelp


cellHelp : GridState -> Parser (Step GridState (List ( Int, Int )))
cellHelp gridState =
    oneOf
        [ succeed (addCells gridState)
            |= int
            |= cellToken
        , succeed (addCells gridState 1)
            |= cellToken
        , succeed (nextLine gridState)
            |. token "$"
        , succeed (Done gridState.cellList)
            |. token "!"
        , succeed (Done gridState.cellList)
            |. end
        , problem "Invalid RLE string. I support only two states, so use `b` and `o` for cells"
        ]


cellToken : Parser CellState
cellToken =
    oneOf
        [ map (\_ -> Dead) (token "b")
        , map (\_ -> Alive) (token "o")
        ]


addCells : GridState -> Int -> CellState -> Step GridState (List ( Int, Int ))
addCells gridState count aliveOrDead =
    case aliveOrDead of
        Alive ->
            let
                newX =
                    gridState.x + count

                xRange =
                    List.range gridState.x (newX - 1)

                cellsToAdd =
                    List.map (\i -> ( i, gridState.y )) xRange

                updatedState =
                    { gridState | x = newX, cellList = gridState.cellList ++ cellsToAdd }
            in
            Loop updatedState

        Dead ->
            let
                newX =
                    gridState.x + count

                updatedState =
                    { gridState | x = newX }
            in
            Loop updatedState


nextLine : GridState -> Step GridState (List ( Int, Int ))
nextLine gridState =
    let
        updatedState =
            { gridState | x = 0, y = gridState.y + 1 }
    in
    Loop updatedState
