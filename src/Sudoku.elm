module Sudoku exposing (..)


import Array exposing (Array)


type Cell
    = Empty
    | Filled Int


type Sudoku = Sudoku (Array Cell)


emptySudoku : Sudoku
emptySudoku =
    Sudoku (Array.repeat 81 Empty)


cells : Sudoku -> List Cell
cells (Sudoku arr) =
    Array.toList arr


updateCell : Sudoku -> Int -> Cell -> Sudoku
updateCell (Sudoku arr) idx newCell =
    Sudoku (Array.set idx newCell arr)


rowByIndex : Int -> Int
rowByIndex idx = idx // 9


colByIndex : Int -> Int
colByIndex idx = modBy 9 idx


boxByIndex : Int -> Int
boxByIndex idx =
    let
        row = rowByIndex idx
        col = colByIndex idx
    in
        (row // 3) * 3 + (col // 3)


inRow : Sudoku -> Int -> List Cell
inRow (Sudoku arr) =
    inSome (Sudoku arr) rowByIndex


inCol : Sudoku -> Int -> List Cell
inCol (Sudoku arr) =
    inSome (Sudoku arr) colByIndex


inBox : Sudoku -> Int -> List Cell
inBox (Sudoku arr) =
    inSome (Sudoku arr) boxByIndex


inSome : Sudoku -> (Int -> Int) -> Int -> List Cell
inSome (Sudoku arr) f n =
    Array.toIndexedList arr
    |> List.filter (\(i, _) -> f i == n)
    |> List.map Tuple.second
    |> List.filter (\c -> c /= Empty)


inAny : Sudoku -> Int -> List Cell
inAny sudoku n =
    List.concat
        [ inRow sudoku (rowByIndex n)
        , inCol sudoku (colByIndex n)
        , inBox sudoku (boxByIndex n)
        ]
    |> uniqueValues


uniqueValues : List a -> List a
uniqueValues lst =
    List.foldl
        (\x acc -> if List.member x acc then acc else x :: acc)
        []
        lst
