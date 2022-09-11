module ContainerUI exposing (..)

import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Items exposing (Container, ItemSlot, itemSlotToString)
import List.Extra


type alias Model =
    { container : Container
    , selectedSlot : Maybe Int
    }


type Msg
    = SelectSlot Int
    | UnselectSlot


init : Container -> Model
init container =
    { container = container, selectedSlot = Nothing }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectSlot slot ->
            { model | selectedSlot = Just slot }

        UnselectSlot ->
            { model | selectedSlot = Nothing }


getSelectedSlot : Model -> Maybe ItemSlot
getSelectedSlot model =
    model.selectedSlot |> Maybe.andThen (\i -> List.Extra.getAt i model.container.items)


viewItem : Maybe ItemSlot -> Int -> Bool -> Html.Html Msg
viewItem itemSlot i isSelected =
    Html.div
        [ Attrs.class "w-12 h-12 border-black flex items-center justify-center border-[1px] rounded-sm text-xs bg-slate-200 hover:bg-slate-400 hover:text-white hover:cursor-pointer"
        , Attrs.class <|
            if isSelected then
                "border-2 bg-slate-500 text-white"

            else
                ""
        , case itemSlot of
            Just _ ->
                Events.onClick (SelectSlot i)

            Nothing ->
                Attrs.class ""
        ]
        [ Html.text <| (Maybe.map itemSlotToString itemSlot |> Maybe.withDefault "") ]


view : Model -> Html.Html Msg
view { container, selectedSlot } =
    let
        count =
            List.length container.items

        full =
            container.items |> List.indexedMap (\i slot -> viewItem (Just slot) i (selectedSlot == Just i))

        empties =
            List.repeat (container.capacity - count) (viewItem Nothing 0 False)
    in
    Html.div [ Attrs.class "grid grid-cols-6 gap-2 w-fit" ]
        (full ++ empties)
