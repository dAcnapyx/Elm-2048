module Main exposing (..)

import Array exposing (Array)
import Time exposing (Time)
import Grid exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Keyboard


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type GameState
    = Playing
    | GameOver
    | Win


type alias Model =
    { grid : Grid
    , score : Int
    , moveScore : Int
    , bestScore : Int
    , state : GameState
    }


type Direction
    = Left
    | Up
    | Right
    | Down
    | None


type DirectionList
    = Empty
    | Node Direction DirectionList


type Msg
    = InitBoard ( ( Int, Int, Int ), ( Int, Int, Int ) )
    | AddNewTile ( Int, Int, Int )
    | Slide Direction
    | AnyOtherKey
    | NewGame


stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "./assets/main.css"
            ]

        children =
            []
    in
        node tag attrs children


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitBoard ( tile1, tile2 ) ->
            let
                newGrid =
                    model.grid
                        |> Grid.addTile tile1
                        |> Grid.addTile tile2
            in
                ( { model | grid = newGrid }, Cmd.none )

        AddNewTile tile ->
            let
                newGrid =
                    model.grid |> Grid.addTile tile

                newModel =
                    ({ model | grid = newGrid })

                newState =
                    checkGameStatus newModel
            in
                ( { model | grid = newGrid, state = newState }, Cmd.none )

        Slide direction ->
            let
                newModel =
                    slide direction model

                newTileCmd =
                    if newModel.grid == model.grid then
                        Cmd.none
                    else
                        Random.generate AddNewTile (newRandomTile newModel.grid)
            in
                ( newModel, newTileCmd )

        AnyOtherKey ->
            ( model, Cmd.none )

        NewGame ->
            newGame model


checkGameStatus : Model -> GameState
checkGameStatus model =
    let
        up =
            move Up model

        down =
            move Down model

        left =
            move Left model

        right =
            move Right model

        gridUp =
            Tuple.first up

        gridDown =
            Tuple.first down

        gridLeft =
            Tuple.first left

        gridRight =
            Tuple.first right

        isGameOver =
            if model.grid == gridUp && model.grid == gridDown && model.grid == gridLeft && model.grid == gridRight then
                True
            else
                False

        isWin =
            Grid.checkWin model.grid

        newStatus =
            if isGameOver then
                GameOver
            else if isWin then
                Win
            else
                Playing
    in
        newStatus


move : Direction -> Model -> ( Grid, ( Direction, ( Int, Int ) ) )
move direction model =
    let
        newMoveScore =
            case direction of
                Up ->
                    Grid.collapse Grid.Up model.grid

                Down ->
                    Grid.collapse Grid.Down model.grid

                Left ->
                    Grid.collapse Grid.Left model.grid

                Right ->
                    Grid.collapse Grid.Right model.grid

                None ->
                    ( model.grid, 0 )

        isNewBest =
            if ((Tuple.second newMoveScore + model.score) > model.bestScore) then
                model.score + Tuple.second newMoveScore
            else
                model.bestScore

        newGrid =
            Tuple.first newMoveScore

        rowScore =
            Tuple.second newMoveScore
    in
        ( newGrid, ( direction, ( rowScore, isNewBest ) ) )


slide : Direction -> Model -> Model
slide direction model =
    let
        newMove =
            move direction model

        newGrid =
            Tuple.first newMove

        rowScore =
            Tuple.first (Tuple.second (Tuple.second newMove))

        isNewBest =
            Tuple.second (Tuple.second (Tuple.second newMove))
    in
        if (newGrid == model.grid) then
            model
        else
            { model
                | grid = newGrid
                , score = model.score + rowScore
                , bestScore = isNewBest
            }


view : Model -> Html Msg
view model =
    let
        rows =
            Grid.rowsFromGrid model.grid
                |> Array.map genRow
                |> Array.toList

        score =
            model.score

        bestScore =
            model.bestScore

        overlay =
            case model.state of
                Playing ->
                    div [] []

                Win ->
                    div [ class "overlay" ]
                        [ span [ class "win" ] [ text "Win" ] ]

                GameOver ->
                    div [ class "overlay" ]
                        [ span [ class "game-over" ] [ text "Game Over" ] ]
    in
        div [ class "container" ]
            [ stylesheet
            , div
                [ class "heading" ]
                [ h1 [ class "title" ] [ text "2048" ]
                , div [ class "scores-container" ]
                    [ div [ class "current-score" ] [ text (toString score) ]
                    , div [ class "best-score" ] [ text (toString bestScore) ]
                    ]
                ]
            , div [ class "game-controls" ]
                [ a [ onClick NewGame, class "brown-button" ] [ text "New Game" ]
                , p [ class "game-info" ] [ text "Join the numbers and get to the 2048 tile!" ]
                ]
            , div [ class "grid-container" ]
                [ overlay
                , div
                    [ class "grid" ]
                    rows
                ]
            ]


genRow : Array Int -> Html Msg
genRow row =
    let
        listRow =
            Array.toList row

        tiles =
            List.map genTile
                listRow
    in
        div [ class "row" ] tiles


genTile : Int -> Html Msg
genTile value =
    div [ class ("tile tile-" ++ toString value) ] [ text (toString value) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    keyPressed


keyPressed : Sub Msg
keyPressed =
    Keyboard.downs
        (\keyCode ->
            if keyCode == 37 then
                Slide Left
            else if keyCode == 38 then
                Slide Up
            else if keyCode == 39 then
                Slide Right
            else if keyCode == 40 then
                Slide Down
            else
                AnyOtherKey
        )


newRandomTile : Grid -> Random.Generator ( Int, Int, Int )
newRandomTile grid =
    let
        two_four =
            Random.map <|
                \num ->
                    if num == 0 then
                        2
                    else
                        4

        emptyTile index =
            List.drop index (Grid.emptyTiles grid)
                |> List.take 1
                |> List.head
                |> Maybe.withDefault ( 9, 9 )
    in
        Random.map2
            (\( x, y ) value -> ( x, y, value ))
            (Random.map emptyTile <|
                Random.int 0 <|
                    (List.length <| Grid.emptyTiles grid)
                        - 1
            )
            (two_four <| Random.int 0 1)


newGame : Model -> ( Model, Cmd Msg )
newGame model =
    let
        newGrid =
            Grid.init
    in
        ( { grid = newGrid
          , score = 0
          , moveScore = 0
          , bestScore = model.bestScore
          , state = Playing
          }
        , Random.generate InitBoard <| Random.pair (newRandomTile newGrid) (newRandomTile newGrid)
        )


init : ( Model, Cmd Msg )
init =
    let
        grid =
            Grid.init
    in
        ( { grid = grid
          , score = 0
          , moveScore = 0
          , bestScore = 0
          , state = Playing
          }
        , Random.generate InitBoard <| Random.pair (newRandomTile grid) (newRandomTile grid)
        )
