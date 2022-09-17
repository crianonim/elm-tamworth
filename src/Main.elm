module Main exposing (Model, Msg, main)

import Browser
import ContainerTransferUI
import ContainerUI
import Farming
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import Items
import Pocket


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { farming : Farming.Model
    , inventory : ContainerUI.Model
    , transfer : ContainerTransferUI.Model
    , pocket : Pocket.Model
    }


init : Model
init =
    { farming = Farming.init
    , inventory = ContainerUI.init Items.exampleContainer
    , transfer = ContainerTransferUI.init Items.exampleContainer Items.exampleContainer2
    , pocket = Pocket.init Items.exampleContainer
    }


type Msg
    = UpdateFarming Farming.Msg
    | UpdateInventory ContainerUI.Msg
    | UpdateTransfer ContainerTransferUI.Msg
    | UpdatePocket Pocket.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateFarming msgFarming ->
            { model | farming = Farming.update msgFarming model.farming }

        UpdateInventory msgContainer ->
            { model | inventory = ContainerUI.update msgContainer model.inventory }

        UpdateTransfer transMsg ->
            { model | transfer = ContainerTransferUI.update transMsg model.transfer }

        UpdatePocket pocketMsg ->
            { model | pocket = Pocket.update pocketMsg model.pocket }


view : Model -> Html Msg
view model =
    let
        ( c1, c2 ) =
            Items.safeMoveBetweenContainers (Items.plantItem Items.Wheat 4) Items.exampleContainer Items.exampleContainer2
    in
    Html.div [ Attrs.class "h-full" ]
        [ --, Farming.view model.farming |> Html.map UpdateFarming
          --, viewContainerTest Items.exampleContainer
          --, viewContainerTest Items.exampleContainer2
          --, viewContainerTest c1
          --, viewContainerTest c2
          Pocket.view model.pocket |> Html.map UpdatePocket

        --, Html.div [] [ Html.text "Inventory" ]
        --, ContainerUI.view model.inventory |> Html.map UpdateInventory
        --, ContainerTransferUI.view model.transfer |> Html.map UpdateTransfer
        --, viewContainer (Items.exampleContainer |> Items.addItemToContainer (Items.single Items.Hoe))
        --, viewContainer (Items.exampleContainer |> Items.safeRemoveItemFromContainer (Items.plantItem Items.Wheat 100))
        --, viewContainer (Items.exampleContainer |> Items.safeRemoveItemFromContainer (Items.single Items.Hoe))
        --, viewContainer (Items.exampleContainer |> Items.addItemToContainer (Items.seedOf Items.Wheat 168))
        --, viewContainer (Items.exampleContainer |> Items.addItemToContainer (Items.plantItem Items.Wheat 168))
        --, Html.text Items.checkNumbers
        ]


viewContainerTest container =
    Html.div [] [ Html.text (String.join ", " (List.map Items.itemSlotToString container.items)) ]
