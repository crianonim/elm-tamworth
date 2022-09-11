module ContainerTransferUI exposing (..)

import ContainerUI
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Items exposing (Container)
import List.Extra
import Util


type alias Model =
    { first : ContainerUI.Model
    , second : ContainerUI.Model
    , selected : Maybe ContainerUI.Model
    }


type Msg
    = UpdateFirst ContainerUI.Msg
    | UpdateSecond ContainerUI.Msg
    | TransferSlotAll ContainerUI.Model


init : Container -> Container -> Model
init first second =
    { first = ContainerUI.init first, second = ContainerUI.init second, selected = Nothing }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateFirst m1 ->
            let
                newFirst =
                    ContainerUI.update m1 model.first

                second =
                    model.second

                ( newSelected, newSecond ) =
                    if Util.isJust newFirst.selectedSlot then
                        ( Just newFirst, { second | selectedSlot = Nothing } )

                    else
                        ( model.selected, second )
            in
            { model | first = newFirst, selected = newSelected, second = newSecond }

        UpdateSecond m2 ->
            let
                newSecond =
                    ContainerUI.update m2 model.second

                first =
                    model.first

                ( newSelected, newFirst ) =
                    if Util.isJust newSecond.selectedSlot then
                        ( Just newSecond, { first | selectedSlot = Nothing } )

                    else
                        ( model.selected, first )
            in
            { model | first = newFirst, selected = newSelected, second = newSecond }

        TransferSlotAll source ->
            if source == model.first then
                case source.selectedSlot of
                    Just slot ->
                        let
                            ( firstContainer, secondContainer ) =
                                Items.safeMoveSlotId slot model.first.container model.second.container
                        in
                        { model | first = ContainerUI.init firstContainer, second = ContainerUI.init secondContainer, selected = Nothing }

                    Nothing ->
                        model

            else
                case source.selectedSlot of
                    Just slot ->
                        let
                            ( secondContainer, firstContainer ) =
                                Items.safeMoveSlotId slot model.second.container model.first.container
                        in
                        { model | first = ContainerUI.init firstContainer, second = ContainerUI.init secondContainer, selected = Nothing }

                    Nothing ->
                        model


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div [ Attrs.class "flex items-center justify-center " ]
            [ Html.text "Transfer"
            , Html.button
                [ case model.selected of
                    Just source ->
                        Events.onClick <| TransferSlotAll source

                    Nothing ->
                        Attrs.disabled True
                , Attrs.class "border-2"
                ]
                [ Html.text ">>>" ]
            ]
        , Html.div [ Attrs.class "items-start flex flex-row gap-8" ]
            [ ContainerUI.view model.first |> Html.map UpdateFirst
            , ContainerUI.view model.second |> Html.map UpdateSecond
            ]
        ]
