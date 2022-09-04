module Main exposing (Model, Msg, main)

import Browser
import Farming
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { farming : Farming.Model }


init : Model
init =
    { farming = Farming.init }


type Msg
    = UpdateFarming Farming.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateFarming msgFarming ->
            { model | farming = Farming.update msgFarming model.farming }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ Attrs.class "h-full grid place-items-center" ]
            []
        , Farming.view model.farming |> Html.map UpdateFarming
        ]
