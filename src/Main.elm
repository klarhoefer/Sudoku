module Main exposing (..)


import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task

import Sudoku exposing (..)
import Utils exposing (..)


type Msg
    = CellChanged Int String
    | CellKey Int String
    | CellFocus Int
    | Solve
    | NoOp


type alias Model =
    { sudoku : Sudoku
    , taken : List Int
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { sudoku = emptySudoku, taken = [] }, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CellChanged n newVal ->
            let
                updatedCell =
                    case String.toInt newVal of
                        Just v ->
                            if v >= 1 && v <= 9 then
                                if List.member v model.taken then
                                    Empty
                                else
                                    Filled v
                            else
                                Empty
                        Nothing ->
                            Empty
            in
                ( {model | sudoku = updateCell model.sudoku n updatedCell}, Cmd.none )

        CellKey n k ->
            let
                target =
                    case k of
                        "ArrowUp" ->
                            if n < 9 then Nothing else Just (n - 9)
                        "ArrowDown" ->
                            if n > 71 then Nothing else Just (n + 9)
                        "ArrowLeft" ->
                            if modBy 9 n == 0 then Nothing else Just (n - 1)
                        "ArrowRight" ->
                            if modBy 9 n == 8 then Nothing else Just (n + 1)
                        _ -> Nothing
            in
                ( model
                , case target of
                    Nothing -> Cmd.none
                    Just t -> Task.attempt (\_ -> NoOp) (Dom.focus ("inp_" ++ String.fromInt t))
                )

        CellFocus n ->
            let
                cells = inAny model.sudoku n
                    |> List.map (\c -> case c of
                        Empty -> 0
                        Filled v -> v)
                    |> Debug.log "Cells taken"
            in
                ( {model| taken = cells}, Cmd.none )

        Solve -> --( model, Task.perform (\_ -> Solve) (Process.sleep 10) )
            ( {model | sudoku = case solve model.sudoku of
                Just solved -> solved
                Nothing -> model.sudoku
            }, Cmd.none)

        NoOp -> ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "sudoku-container" ]
        [ div [ class "hdr-btn" ]
            [ h2 [] [ text "Sudoku" ]
            , button [ onClick Solve ] [ text "Solve" ]
            ]
        , div [ class "sudoku-grid" ]
            (List.indexedMap viewCell (cells model.sudoku))
        ]


viewCell : Int -> Cell -> Html Msg
viewCell n cell =
    let
        val = case cell of
            Empty -> ""
            Filled v -> String.fromInt v
    in
        div [ class "sudoku-cell" ]
            [ input [ type_ "text"
                    , id ("inp_" ++ (String.fromInt n))
                    , maxlength 1
                    , value val
                    , onInput (CellChanged n)
                    , onKeyUp (CellKey n)
                    , onFocus (CellFocus n)
                    ] []
            ]
