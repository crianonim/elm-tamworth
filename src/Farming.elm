module Farming exposing (..)

import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events as Events
import List.Extra


type alias Model =
    { soils : List Soil
    , selectedSoilId : Maybe Int
    , inventory : Inventory
    }


type Msg
    = ChooseSoilAction Int SoilAction
    | SelectSoil Int
    | Turn


type SoilAction
    = SATill
    | SAWater
    | SAPlant PlantKind
    | SAClear
    | SAHarvest PlantGrowing


init : Model
init =
    { soils = sampleSoils
    , selectedSoilId = Nothing
    , inventory = [ ( SeedItem Onion, 3 ), ( PlantItem Wheat, 2 ) ]
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChooseSoilAction i action ->
            List.Extra.getAt i model.soils
                |> Maybe.map
                    (\s -> { s | top = updateSoilTop action s.top })
                |> Maybe.map2 (\m v -> { m | soils = List.Extra.setAt i v m.soils }) (soilTopActionPreRequisite action model)
                |> Maybe.withDefault model

        SelectSoil soil ->
            { model | selectedSoilId = Just soil }

        Turn ->
            { model | soils = List.map processSoilTurn model.soils }


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
            "Stone"


type SoilTop
    = Empty
    | Tilled Bool
    | Planted PlantGrowing


type alias PlantGrowing =
    { stage : Float
    , watered : Bool
    , plantKind : PlantKind
    }


type PlantKind
    = Onion
    | Wheat


type Item
    = PlantItem PlantKind
    | SeedItem PlantKind


type alias Inventory =
    List ( Item, Int )


getItemCount : Item -> Inventory -> Int
getItemCount item inventory =
    List.Extra.find
        (\i -> item == Tuple.first i)
        inventory
        |> Maybe.map Tuple.second
        |> Maybe.withDefault 0


updateItemCount : Item -> (Int -> Int) -> Inventory -> Inventory
updateItemCount item function inventory =
    let
        updated =
            List.map
                (\( i, a ) ->
                    if item == i then
                        ( i, function a )

                    else
                        ( i, a )
                )
                inventory
    in
    if inventory == updated then
        inventory ++ [ ( item, function 0 ) ]

    else
        updated


getSeeds : Inventory -> List ( PlantKind, Int )
getSeeds inventory =
    inventory
        |> List.filterMap
            (\( i, a ) ->
                case i of
                    SeedItem x ->
                        Just ( x, a )

                    _ ->
                        Nothing
            )


soilTopActionPreRequisite : SoilAction -> Model -> Maybe Model
soilTopActionPreRequisite soilAction model =
    case soilAction of
        SATill ->
            Just model

        SAWater ->
            Just model

        SAPlant plantKind ->
            if getItemCount (SeedItem plantKind) model.inventory > 0 then
                Just { model | inventory = updateItemCount (SeedItem plantKind) (\x -> x - 1) model.inventory }

            else
                Nothing

        SAClear ->
            Just model

        SAHarvest { plantKind } ->
            Just { model | inventory = updateItemCount (PlantItem plantKind) (\x -> x + 2) model.inventory }


updateSoilTop : SoilAction -> SoilTop -> SoilTop
updateSoilTop soilAction soilTop =
    case soilAction of
        SATill ->
            Tilled False

        SAWater ->
            case soilTop of
                Planted plant ->
                    Planted { plant | watered = True }

                Tilled _ ->
                    Tilled True

                _ ->
                    soilTop

        SAPlant plant ->
            Planted
                { stage = 0
                , watered = False
                , plantKind = plant
                }

        SAClear ->
            Empty

        SAHarvest { watered } ->
            Tilled watered


plantKindToString : PlantKind -> String
plantKindToString plantKind =
    case plantKind of
        Onion ->
            "Onion"

        Wheat ->
            "Wheat"


isSoilTillable : Soil -> Bool
isSoilTillable soil =
    let
        isKindTillable =
            case soil.kind of
                Dirt ->
                    True

                _ ->
                    False

        isTopTillable =
            case soil.top of
                Empty ->
                    True

                Tilled _ ->
                    False

                Planted _ ->
                    True
    in
    isKindTillable && isTopTillable


viewSoil : Maybe Int -> Int -> Soil -> Html Msg
viewSoil mSelected i soil =
    let
        attrs =
            if Just i == mSelected then
                [ Attrs.disabled True
                , Attrs.class "border bg-slate-200"
                ]

            else
                [ Events.onClick <| SelectSoil i
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


stageToString : Float -> String
stageToString stage =
    (if stage < 1 then
        "Germinating"

     else if stage < 2 then
        "Sapling"

     else if stage < 3 then
        "Young"

     else
        "Mature"
    )
        ++ " ("
        ++ String.fromInt (round ((stage - (toFloat <| truncate stage)) * 100))
        ++ "%) "


viewSoilTop : SoilTop -> Html Msg
viewSoilTop soilTop =
    case soilTop of
        Empty ->
            text "empty"

        Tilled watered ->
            span [] [ text "tilled ", text <| isWateredString watered ]

        Planted plantGrowing ->
            viewPlantGrowing plantGrowing


isWateredString : Bool -> String
isWateredString watered =
    if watered then
        "watered"

    else
        "thirsty"


processSoilTurn : Soil -> Soil
processSoilTurn soil =
    { soil
        | top =
            case soil.top of
                Planted planted ->
                    Planted <| growPlant planted

                a ->
                    a
    }


growPlant : PlantGrowing -> PlantGrowing
growPlant plantGrowing =
    let
        speed =
            case plantGrowing.plantKind of
                Onion ->
                    0.3

                Wheat ->
                    0.2
    in
    if plantGrowing.watered then
        { plantGrowing
            | stage = plantGrowing.stage + speed
            , watered = False
        }

    else
        plantGrowing


viewPlantGrowing : PlantGrowing -> Html Msg
viewPlantGrowing plantGrowing =
    div [ Attrs.class "flex flex-row gap-2" ]
        [ span [] [ text <| plantKindToString plantGrowing.plantKind ]
        , span [] [ text <| stageToString plantGrowing.stage ]
        , span []
            [ text <| isWateredString plantGrowing.watered
            ]
        ]


sampleSoils : List Soil
sampleSoils =
    [ Soil Dirt Empty
    , Soil Clay Empty
    , Soil Dirt (Tilled False)
    , Soil Dirt (Planted (PlantGrowing 0 True Onion))
    , Soil Dirt (Planted (PlantGrowing 1 False Wheat))
    , Soil Stone Empty
    ]


itemToString : Item -> String
itemToString item =
    case item of
        PlantItem plantKind ->
            plantKindToString plantKind

        SeedItem plantKind ->
            "Seed of " ++ plantKindToString plantKind


viewInventory : Inventory -> Html Msg
viewInventory inventory =
    div [] (List.map (\( i, a ) -> text <| " (" ++ itemToString i ++ ", " ++ String.fromInt a ++ ") ") inventory)


viewSeedButtons : Inventory -> Int -> Html Msg
viewSeedButtons inventory i =
    div []
        (getSeeds inventory
            |> List.map
                (\( seed, amount ) ->
                    if amount > 0 then
                        button [ Events.onClick <| ChooseSoilAction i <| SAPlant seed ] [ text "Plant ", text <| plantKindToString seed ]

                    else
                        text ""
                )
        )


viewSelectedSoilActions : Model -> Int -> Html Msg
viewSelectedSoilActions { soils, inventory } i =
    List.Extra.getAt i soils
        |> Maybe.map
            (\soil ->
                div [ Attrs.class "flex flex-row gap-2" ]
                    ([ ( case soil.top of
                            Tilled False ->
                                True

                            Planted plant ->
                                not plant.watered

                            _ ->
                                False
                       , button [ Events.onClick <| ChooseSoilAction i SAWater ] [ text "Water" ]
                       )
                     , ( case soil.top of
                            Empty ->
                                False

                            _ ->
                                True
                       , button [ Events.onClick <| ChooseSoilAction i SAClear ] [ text "Clear" ]
                       )
                     , ( isSoilTillable soil
                       , button [ Events.onClick <| ChooseSoilAction i SATill ] [ text "Till" ]
                       )
                     , ( case soil.top of
                            Tilled _ ->
                                True

                            _ ->
                                False
                       , viewSeedButtons inventory i
                       )
                     , ( case soil.top of
                            Planted { stage } ->
                                stage > 3

                            _ ->
                                False
                       , case soil.top of
                            Planted planted ->
                                button [ Events.onClick <| ChooseSoilAction i (SAHarvest planted) ] [ text "Harvest" ]

                            _ ->
                                text ""
                       )
                     ]
                        |> List.filter Tuple.first
                        |> List.map Tuple.second
                    )
            )
        |> Maybe.withDefault (text "")


view : Model -> Html Msg
view model =
    div []
        ([ button [ Events.onClick Turn ] [ text "Turn" ], viewInventory model.inventory ]
            ++ [ htmlCond
                    (viewSelectedSoilActions model)
                    model.selectedSoilId
               ]
            ++ List.indexedMap (viewSoil model.selectedSoilId) model.soils
        )


htmlCond : (a -> Html Msg) -> Maybe a -> Html Msg
htmlCond fn m =
    case m of
        Nothing ->
            text ""

        Just x ->
            fn x
