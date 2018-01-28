module Grid
    exposing
        ( Grid
        , Direction(..)
        , init
        , rowsFromGrid
        , addTile
        , collapse
        , emptyTiles
        , checkWin
        )

import Matrix exposing (..)
import Array exposing (Array)


type Grid
    = Grid (Matrix Int)


type Direction
    = Left
    | Up
    | Right
    | Down


getCol : Int -> Grid -> Array Int
getCol n (Grid matrix) =
    Matrix.getCol n matrix


rotateLeft : Grid -> Grid
rotateLeft grid =
    let
        col_1 =
            getCol 0 grid

        col_2 =
            getCol 1 grid

        col_3 =
            getCol 2 grid

        col_4 =
            getCol 3 grid
    in
        gridFromRows <| Array.fromList [ col_4, col_3, col_2, col_1 ]


rotateRight : Grid -> Grid
rotateRight grid =
    let
        col_1 =
            reverseRow (getCol 0 grid)

        col_2 =
            reverseRow (getCol 1 grid)

        col_3 =
            reverseRow (getCol 2 grid)

        col_4 =
            reverseRow (getCol 3 grid)
    in
        gridFromRows <| Array.fromList [ col_1, col_2, col_3, col_4 ]


reverseRow : Array Int -> Array Int
reverseRow row =
    Array.toList row
        |> List.reverse
        |> Array.fromList


addTile : ( Int, Int, Int ) -> Grid -> Grid
addTile tile grid =
    case tile of
        ( x, y, value ) ->
            gridFromRows <|
                Array.indexedMap
                    (\ri row ->
                        if ri == x then
                            Array.indexedMap
                                (\ci column ->
                                    if ci == y then
                                        value
                                    else
                                        column
                                )
                                row
                        else
                            row
                    )
                    (rowsFromGrid grid)


gridFromRows : Array (Array Int) -> Grid
gridFromRows rows =
    Grid (Matrix.wrap rows)


rowsFromGrid : Grid -> Array (Array Int)
rowsFromGrid (Grid matrix) =
    Matrix.unwrap matrix


rowsAsList : Grid -> List (List Int)
rowsAsList (Grid matrix) =
    Matrix.unwrap matrix
        |> Array.map
            Array.toList
        |> Array.toList


emptyTiles : Grid -> List ( Int, Int )
emptyTiles (Grid matrix) =
    List.filterMap identity <|
        List.concatMap identity <|
            List.indexedMap
                (\ri row ->
                    List.indexedMap
                        (\ci cell ->
                            if cell == 0 then
                                Just ( ri, ci )
                            else
                                Nothing
                        )
                        row
                )
                (rowsAsList (Grid matrix))


checkWin : Grid -> Bool
checkWin (Grid matrix) =
    let
        list2048 =
            List.filterMap identity <|
                List.concatMap identity <|
                    List.indexedMap
                        (\ri row ->
                            List.indexedMap
                                (\ci cell ->
                                    if cell == 2048 then
                                        Just True
                                    else
                                        Nothing
                                )
                                row
                        )
                        (rowsAsList (Grid matrix))

        isFound =
            if List.length list2048 > 0 then
                True
            else
                False
    in
        isFound


collapseRow : List Int -> ( List Int, Int )
collapseRow row =
    let
        collapsed row =
            case row of
                [ a, b, c, d ] ->
                    if a == b && c == d && a /= 0 && c /= 0 then
                        [ a * 2 + c * 2, 0, 0, a * 2, c * 2 ]
                    else if c == d && c /= 0 then
                        [ c * 2, 0, a, b, c * 2 ]
                    else if b == d && b /= 0 && c == 0 then
                        [ b * 2, 0, 0, a, b * 2 ]
                    else if b == c && b /= 0 then
                        [ b * 2, 0, a, b * 2, d ]
                    else if a == d && a /= 0 && b == 0 && c == 0 then
                        [ a * 2, 0, 0, 0, a * 2 ]
                    else if a == c && a /= 0 && b == 0 then
                        [ a * 2, 0, 0, a * 2, d ]
                    else if a == b && a /= 0 then
                        [ a * 2, 0, a * 2, c, d ]
                    else
                        [ 0, a, b, c, d ]

                _ ->
                    row

        rowScore =
            case (List.head (collapsed row)) of
                Nothing ->
                    0

                Just score ->
                    score

        newRow =
            case (List.tail (collapsed row)) of
                Nothing ->
                    [ 0, 0, 0, 0 ]

                Just row ->
                    row
    in
        ( newRow
            |> List.partition ((==) 0)
            |> uncurry List.append
        , rowScore
        )


collapse : Direction -> Grid -> ( Grid, Int )
collapse direction grid =
    let
        rotatedGrid =
            (case direction of
                Up ->
                    grid
                        |> rotateRight
                        |> rowsAsList

                Right ->
                    grid
                        |> rowsAsList

                Down ->
                    grid
                        |> rotateLeft
                        |> rowsAsList

                Left ->
                    grid
                        |> rowsAsList
                        |> List.map List.reverse
            )

        rowsAndScores =
            List.map collapseRow <| rotatedGrid

        slidRotatedGrid =
            List.map Tuple.first rowsAndScores

        scoreGained =
            List.sum <| List.map Tuple.second rowsAndScores

        newGrid =
            (case direction of
                Up ->
                    slidRotatedGrid
                        |> listToGrid
                        |> rotateLeft

                Right ->
                    slidRotatedGrid
                        |> listToGrid

                Down ->
                    slidRotatedGrid
                        |> listToGrid
                        |> rotateRight

                Left ->
                    slidRotatedGrid
                        |> List.map List.reverse
                        |> listToGrid
            )

        rowScore =
            scoreGained
    in
        ( newGrid, rowScore )


listToGrid : List (List Int) -> Grid
listToGrid list =
    let
        newArray =
            List.map (Array.fromList)
                list
                |> Array.fromList
    in
        newArray
            |> gridFromRows


init : Grid
init =
    Grid
        (Matrix.init 4 4 0)
