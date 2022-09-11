module Main exposing (Model, Msg, main)

import Browser
import Farming
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import Items


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
    let
        ( c1, c2 ) =
            Items.safeMoveBetweenContainers (Items.plantItem Items.Wheat 4) Items.exampleContainer Items.exampleContainer2
    in
    Html.div []
        [ Html.div [ Attrs.class "h-full grid place-items-center" ]
            []
        , Farming.view model.farming |> Html.map UpdateFarming
        , viewContainer Items.exampleContainer
        , viewContainer Items.exampleContainer2
        , viewContainer c1
        , viewContainer c2

        --, viewContainer (Items.exampleContainer |> Items.addItemToContainer (Items.single Items.Hoe))
        --, viewContainer (Items.exampleContainer |> Items.safeRemoveItemFromContainer (Items.plantItem Items.Wheat 100))
        --, viewContainer (Items.exampleContainer |> Items.safeRemoveItemFromContainer (Items.single Items.Hoe))
        --, viewContainer (Items.exampleContainer |> Items.addItemToContainer (Items.seedOf Items.Wheat 168))
        --, viewContainer (Items.exampleContainer |> Items.addItemToContainer (Items.plantItem Items.Wheat 168))
        --, Html.text Items.checkNumbers
        ]


viewContainer container =
    Html.div [] [ Html.text (String.join ", " (List.map Items.itemSlotToString container.items)) ]
