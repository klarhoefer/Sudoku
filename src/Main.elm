module Main exposing (..)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = Noop


type Cell
    = Empty
    | Filled Int


type alias Model =
    { field: List Cell
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
    ( { field = List.repeat 81 Empty }, Cmd.none )


updateField : List Cell -> List Cell
updateField field =
    field
        |> List.map
            (\cell ->
                case cell of
                    Empty ->
                        Filled 1

                    Filled n ->
                        Filled (n+1)
            )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Noop ->
            ( {model | field = updateField model.field}, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "sudoku-container" ]
        [ h2 [] [ text "Sudoku" ]
        , div [ class "sudoku-grid" ]
            (List.map viewCell model.field)
        ]


slice : Int -> Int -> List a -> List a
slice start end list =
    list
        |> List.drop start
        |> List.take (end - start)


viewCell : Cell -> Html Msg
viewCell cell =
    div [ class "sudoku-cell" ]
        [ input [ type_ "text", maxlength 1 ] []
        ]
