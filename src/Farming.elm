module Farming exposing (..)

import Html exposing (..)
import Html.Attributes as Attrs exposing (class)
import Html.Events as Events


type alias Model =
    { soil : List Soil
    , selectedSoil : Maybe Soil
    }


type Msg
    = ChooseSoilAction SoilAction
    | SelectSoil Soil


type SoilAction
    = SATill
    | SAWater


init : Model
init =
    { soil = sampleSoils
    , selectedSoil = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChooseSoilAction action ->
            case model.selectedSoil of
                Nothing ->
                    model

                Just selected ->
                    let
                        nSelected =
                            { selected | top = updateSoilTop action selected.top }
                    in
                    { model
                        | selectedSoil = Just nSelected
                        , soil =
                            List.map
                                (\s ->
                                    if s == selected then
                                        nSelected

                                    else
                                        s
                                )
                                model.soil
                    }

        SelectSoil soil ->
            { model | selectedSoil = Just soil }


type alias Soil =
    { kind : SoilKind
    , top : SoilTop
    }


type SoilKind
    = Dirt
    | Clay
    | Stone


soilKindToString : SoilKind -> String
soilKindToString soilKind =
    case soilKind of
        Dirt ->
            "Dirt"

        Clay ->
            "Clay"

        Stone ->
            "S§tone"


type SoilTop
    = Empty
    | Tilled
    | Planted PlantGrowing


type alias PlantGrowing =
    { stage : Int
    , watered : Bool
    , plantKind : PlantKind
    }


type PlantKind
    = Onion
    | Wheat


updateSoilTop : SoilAction -> SoilTop -> SoilTop
updateSoilTop soilAction soilTop =
    case soilAction of
        SATill ->
            Tilled

        SAWater ->
            case soilTop of
                Planted plant ->
                    Planted { plant | watered = True }

                _ ->
                    soilTop


plantKindToString : PlantKind -> String
plantKindToString plantKind =
    case plantKind of
        Onion ->
            "Onion"

        Wheat ->
            "Wheat"


viewSoil : Maybe Soil -> Soil -> Html Msg
viewSoil mSelected soil =
    let
        attrs =
            if Just soil == mSelected then
                [ Attrs.disabled True
                , Attrs.class "border bg-slate-200"
                ]

            else
                [ Events.onClick <| SelectSoil soil
                , Attrs.class "bg-red"
                ]
    in
    div [ Attrs.class "flex flex-row gap-2" ]
        [ button attrs [ text "Select" ]
        , div ([ Attrs.class "flex flex-row gap-2" ] ++ attrs)
            [ text "Soil Kind:"
            , span [] [ text <| soilKindToString soil.kind ]
            , viewSoilTop soil.top
            ]
        ]


viewSoilTop : SoilTop -> Html Msg
viewSoilTop soilTop =
    case soilTop of
        Empty ->
            text "empty"

        Tilled ->
            text "tilled"

        Planted plantGrowing ->
            viewPlantGrowing plantGrowing


viewPlantGrowing : PlantGrowing -> Html Msg
viewPlantGrowing plantGrowing =
    div [ Attrs.class "flex flex-row gap-2" ]
        [ span [] [ text <| plantKindToString plantGrowing.plantKind ]
        , span [] [ text <| String.fromInt plantGrowing.stage ]
        , span []
            [ text <|
                if plantGrowing.watered then
                    "watered"

                else
                    "thirsty"
            ]
        ]


sampleSoils : List Soil
sampleSoils =
    [ Soil Dirt Empty
    , Soil Clay Empty
    , Soil Dirt Tilled
    , Soil Dirt (Planted (PlantGrowing 1 True Onion))
    , Soil Dirt (Planted (PlantGrowing 1 False Wheat))
    ]


viewSelectedSoilActions : Soil -> Html Msg
viewSelectedSoilActions soil =
    div [ Attrs.class "flex flex-row gap-2" ]
        [ button [ Events.onClick <| ChooseSoilAction SAWater ] [ text "Water" ]
        , button [ Events.onClick <| ChooseSoilAction SATill ] [ text "Till" ]
        ]


view : Model -> Html Msg
view model =
    div []
        ([ htmlCond
            viewSelectedSoilActions
            model.selectedSoil
         ]
            ++ List.map (viewSoil model.selectedSoil) model.soil
        )


htmlCond : (a -> Html Msg) -> Maybe a -> Html Msg
htmlCond fn m =
    case m of
        Nothing ->
            text ""

        Just x ->
            fn x
