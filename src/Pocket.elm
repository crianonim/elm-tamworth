module Pocket exposing (..)

import Array exposing (Array)
import ContainerUI
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Html.Extra
import Items as Items
import Json.Decode as Decode
import List.Extra


type alias Model =
    { container : Array (Maybe Int)
    , selectedIndex : Int
    , inventory : ContainerUI.Model
    , showInventory : Bool
    }


init : Items.Container -> Model
init inventory =
    { container =
        Array.initialize 10 (always Nothing)
            |> Array.set 2 (Just 5)
            |> Array.set 0 (Just 6)
    , selectedIndex = 0
    , inventory = ContainerUI.init inventory
    , showInventory = False
    }


type Msg
    = UpdateInventory ContainerUI.Msg
    | SetPocketItem
    | SelectPocketSlot Int
    | ShowInventoryToggle


getSelectedItem : Model -> Maybe Items.ItemSlot
getSelectedItem model =
    Array.get model.selectedIndex model.container
        |> Maybe.andThen identity
        |> Maybe.andThen (\i -> List.Extra.getAt i model.inventory.container.items)


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInventory containerMsg ->
            { model | inventory = ContainerUI.update containerMsg model.inventory }

        SetPocketItem ->
            { model | container = Array.set model.selectedIndex model.inventory.selectedSlot model.container }

        SelectPocketSlot int ->
            { model | selectedIndex = int }

        ShowInventoryToggle ->
            { model | showInventory = not model.showInventory }


view : Model -> Html Msg
view model =
    Html.div [ Attrs.class "absolute bottom-0 w-full flex flex-col items-center" ]
        [ Html.div []
            [ Html.Extra.viewIf model.showInventory <|
                Html.div []
                    [ ContainerUI.view model.inventory |> Html.map UpdateInventory
                    , Html.div [] [ Html.button [ Events.onClick SetPocketItem ] [ Html.text "Pocket" ] ]
                    ]
            , Html.div [ Attrs.class "flex gap-1" ] [ viewPocket model, Html.button [ Events.onClick ShowInventoryToggle ] [ Html.text "Inv" ] ]
            ]
        ]


viewPocket : Model -> Html Msg
viewPocket model =
    Html.div [ Attrs.class "grid grid-cols-10 gap-2 w-fit" ]
        (List.indexedMap
            (\i slot ->
                let
                    isSelected =
                        i == model.selectedIndex
                in
                Maybe.andThen
                    (\slotId -> List.Extra.getAt slotId model.inventory.container.items)
                    slot
                    |> Maybe.map (\itemSlot -> viewItem (Just itemSlot) i isSelected)
                    |> Maybe.withDefault (viewItem Nothing i isSelected)
            )
            (Array.toList model.container)
        )


viewItem : Maybe Items.ItemSlot -> Int -> Bool -> Html.Html Msg
viewItem itemSlot i isSelected =
    Html.div
        [ Attrs.class "w-12 h-12 border-black flex items-center justify-center border-[1px] rounded-sm text-xs bg-slate-200 hover:bg-slate-400 hover:text-white hover:cursor-pointer"
        , Attrs.class <|
            if isSelected then
                "border-4 bg-slate-400 text-white"

            else
                ""
        , Events.onClick <| SelectPocketSlot i
        , Events.preventDefaultOn "contextmenu" (Decode.succeed ( SelectPocketSlot i, True ))
        ]
        [ Html.text (Maybe.map Items.itemSlotToString itemSlot |> Maybe.withDefault "") ]
