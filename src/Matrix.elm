module Matrix
    exposing
        ( Matrix
        , wrap
        , unwrap
        , getCol
        , init
        )

import Array exposing (Array)


type Matrix a
    = Matrix (Array (Array a))


wrap : Array (Array a) -> Matrix a
wrap matrix =
    Matrix matrix


unwrap : Matrix a -> Array (Array a)
unwrap (Matrix matrix) =
    matrix


getCol : Int -> Matrix a -> Array a
getCol n matrix =
    let
        newArray =
            matrix
                |> unwrap
                |> arrayTraverse identity
                << Array.map (getN n)
    in
        case newArray of
            Nothing ->
                Array.empty

            Just a ->
                a


getN : Int -> Array a -> Maybe a
getN n a =
    let
        len =
            Array.length a
    in
        if n < 0 && abs n <= len then
            Array.get (len + n) a
        else
            Array.get n a


arrayTraverse : (a -> Maybe b) -> Array a -> Maybe (Array b)
arrayTraverse f =
    let
        step e acc =
            case f e of
                Nothing ->
                    Nothing

                Just x ->
                    Maybe.map (Array.push x) acc
    in
        Array.foldl step (Just Array.empty)


init : Int -> Int -> a -> Matrix a
init cols rows val =
    Matrix <|
        Array.repeat cols
            (Array.repeat rows val)
