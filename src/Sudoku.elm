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
