module Utils exposing (..)


import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Decode


key : Decode.Decoder String
key =
    Decode.field "key" Decode.string


onKeyUp : (String -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Decode.map tagger key)
