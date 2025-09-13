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
    | NoOp


type alias Model =
    { sudoku : Sudoku
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sudoku = emptySudoku }, Cmd.none )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CellChanged n newVal ->
            let
                updatedCell =
                    case String.toInt newVal of
                        Just v ->
                            if v >= 1 && v <= 9 then
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

        NoOp -> ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "sudoku-container" ]
        [ h2 [] [ text "Sudoku" ]
        , div [ class "sudoku-grid" ]
            (List.indexedMap viewCell (toCellList model.sudoku))
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
                    ] []
            ]
