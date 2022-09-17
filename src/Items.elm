module Items exposing (..)

---Items

import Html
import Html.Attributes as Attrs
import List.Extra
import Util


type ItemSlot
    = Single SingleItemType
    | Stackable StackableItemType Int


type ItemKind
    = HoeKind
    | WateringCanKind
    | WheatKind
    | CornKind
    | SeedKind PlantItemType
    | NotAnItem


type StackableItemType
    = SeedItem PlantItemType
    | PlantItem PlantItemType


type SingleItemType
    = Hoe
    | WateringCan Int
    | EmptySlot


type PlantItemType
    = Wheat
    | Corn


stackSize =
    32


itemToItemKind : ItemSlot -> ItemKind
itemToItemKind item =
    case item of
        Single singleItemType ->
            case singleItemType of
                Hoe ->
                    HoeKind

                WateringCan _ ->
                    WateringCanKind

                EmptySlot ->
                    NotAnItem

        Stackable stackableItemType _ ->
            case stackableItemType of
                SeedItem plantItemType ->
                    SeedKind plantItemType

                PlantItem plantItemType ->
                    case plantItemType of
                        Wheat ->
                            WheatKind

                        Corn ->
                            CornKind


itemKindToString : ItemKind -> String
itemKindToString itemKind =
    case itemKind of
        HoeKind ->
            "Hoe"

        WateringCanKind ->
            "Watering Can"

        WheatKind ->
            "Wheat"

        CornKind ->
            "Corn"

        SeedKind plantItemType ->
            itemKindToString (itemToItemKind <| stackable (PlantItem plantItemType) 1) ++ " Seed"

        NotAnItem ->
            ""


itemSlotToString : ItemSlot -> String
itemSlotToString item =
    case item of
        Single s ->
            case s of
                WateringCan fullness ->
                    "Watering Can (" ++ String.fromInt fullness ++ ")"

                _ ->
                    itemKindToString (itemToItemKind item)

        Stackable _ amount ->
            itemKindToString (itemToItemKind item) ++ " [" ++ String.fromInt amount ++ "] "


stackable : StackableItemType -> Int -> ItemSlot
stackable stackableItemType int =
    Stackable stackableItemType int


plantItem : PlantItemType -> Int -> ItemSlot
plantItem item a =
    stackable (PlantItem item) a


seedOf : PlantItemType -> Int -> ItemSlot
seedOf item a =
    Stackable (SeedItem item) a


single : SingleItemType -> ItemSlot
single singleItemType =
    Single singleItemType


empty : ItemSlot
empty =
    single EmptySlot


exampleItems =
    [ plantItem Corn 5
    , plantItem Wheat 32
    , plantItem Wheat 1
    , plantItem Wheat 8

    --, seedOf Wheat 12
    , seedOf Corn 3
    , single (WateringCan 5)
    , single Hoe
    ]



--- Containers


type alias Container =
    { items : List ItemSlot
    , capacity : Int
    }


itemCount : ItemSlot -> Int
itemCount item =
    case item of
        Single _ ->
            1

        Stackable _ a ->
            a


itemSlotsInContainer : ItemKind -> Container -> List ItemSlot
itemSlotsInContainer itemKind container =
    container.items
        |> List.filter (itemToItemKind >> (==) itemKind)


itemCountInContainer : ItemKind -> Container -> Int
itemCountInContainer itemKind container =
    itemSlotsInContainer itemKind container
        |> List.map itemCount
        |> List.sum


getFirstSlotOfItem : ItemKind -> Container -> Maybe ItemSlot
getFirstSlotOfItem itemKind container =
    itemSlotsInContainer itemKind container
        |> List.head


addItemToContainer : ItemSlot -> Container -> Container
addItemToContainer itemSlot container =
    let
        addAmountToStackable : Int -> ItemSlot -> ( ItemSlot, Int )
        addAmountToStackable a it =
            case it of
                Single _ ->
                    ( it, 0 )

                Stackable stackableItemType n ->
                    let
                        sum =
                            n + a

                        ( newAmount, leftOver ) =
                            if sum > stackSize then
                                ( stackSize, sum - stackSize )

                            else
                                ( sum, 0 )
                    in
                    ( Stackable stackableItemType newAmount, leftOver )
    in
    case itemSlot of
        Single _ ->
            { container | items = container.items ++ [ itemSlot ] }

        Stackable item a ->
            let
                overflowItemSlot : Int -> List ItemSlot
                overflowItemSlot count =
                    let
                        remainder =
                            modBy stackSize count

                        reminderStack =
                            if remainder /= 0 then
                                [ stackable item remainder ]

                            else
                                []

                        newStacks =
                            List.repeat (count // stackSize) (stackable item stackSize)

                        _ =
                            Debug.log "overflow" ( newStacks, reminderStack )
                    in
                    newStacks ++ reminderStack

                addToItems : List ItemSlot -> ( List ItemSlot, Int )
                addToItems items =
                    case
                        List.foldl
                            (\i ( acc, leftToAdd ) ->
                                if leftToAdd < 1 then
                                    ( acc ++ [ i ], 0 )

                                else if itemToItemKind itemSlot == itemToItemKind i then
                                    let
                                        ( newStack, left ) =
                                            addAmountToStackable leftToAdd i
                                    in
                                    ( acc ++ [ newStack ], left )

                                else
                                    ( acc ++ [ i ], leftToAdd )
                            )
                            ( [], a )
                            items
                    of
                        ( newItems, 0 ) ->
                            ( newItems, 0 )

                        ( newItems, x ) ->
                            ( newItems ++ overflowItemSlot x, 0 )
            in
            { container
                | items =
                    case getFirstSlotOfItem (itemToItemKind itemSlot) container of
                        Nothing ->
                            container.items ++ overflowItemSlot (itemCount itemSlot)

                        Just _ ->
                            addToItems container.items
                                |> Tuple.first
            }


removeItemFromContainer : ItemSlot -> Container -> Maybe Container
removeItemFromContainer itemSlot container =
    let
        hasAmount =
            itemCountInContainer (itemToItemKind itemSlot) container

        toRemove =
            itemCount itemSlot
    in
    if hasAmount >= toRemove then
        Just
            { container
                | items =
                    List.foldr
                        (\i ( acc, leftToRemove ) ->
                            if leftToRemove < 1 then
                                ( acc ++ [ i ], 0 )

                            else if itemToItemKind itemSlot == itemToItemKind i then
                                let
                                    removeAmountFromStackable : Int -> ItemSlot -> ( Maybe ItemSlot, Int )
                                    removeAmountFromStackable n item =
                                        case item of
                                            Single _ ->
                                                ( Nothing, 0 )

                                            Stackable stackableItemType _ ->
                                                let
                                                    leftAfterRemove =
                                                        itemCount item - n
                                                in
                                                ( if leftAfterRemove <= 0 then
                                                    Nothing

                                                  else
                                                    Just <| stackable stackableItemType leftAfterRemove
                                                , if leftAfterRemove < 0 then
                                                    -leftAfterRemove

                                                  else
                                                    0
                                                )

                                    ( newStack, left ) =
                                        removeAmountFromStackable leftToRemove i
                                in
                                ( acc ++ (newStack |> Maybe.map List.singleton |> Maybe.withDefault []), left )

                            else
                                ( acc ++ [ i ], leftToRemove )
                        )
                        ( [], toRemove )
                        container.items
                        |> Tuple.first
                        |> List.reverse
            }

    else
        Nothing


safeRemoveItemFromContainer : ItemSlot -> Container -> Container
safeRemoveItemFromContainer itemSlot container =
    removeItemFromContainer itemSlot container |> Maybe.withDefault container


filterOutItemSlotsWithZero : List ItemSlot -> List ItemSlot
filterOutItemSlotsWithZero items =
    List.filter (itemCount >> (/=) 0) items


moveBetweenContainers : ItemSlot -> Container -> Container -> Maybe ( Container, Container )
moveBetweenContainers itemSlot source destination =
    removeItemFromContainer itemSlot source
        |> Maybe.map (\removed -> ( removed, addItemToContainer itemSlot destination ))


safeMoveBetweenContainers : ItemSlot -> Container -> Container -> ( Container, Container )
safeMoveBetweenContainers itemSlot source destination =
    moveBetweenContainers itemSlot source destination |> Maybe.withDefault ( source, destination )


moveSlotId : Int -> Container -> Container -> Maybe ( Container, Container )
moveSlotId id source destination =
    List.Extra.getAt id source.items
        |> Maybe.map
            (\item ->
                let
                    newDestinationAttempt =
                        addItemToContainer item destination

                    overflowSlot =
                        List.Extra.getAt newDestinationAttempt.capacity newDestinationAttempt.items
                in
                ( { source
                    | items =
                        case overflowSlot of
                            Just slot ->
                                List.Extra.setAt id slot source.items

                            Nothing ->
                                List.Extra.removeAt id source.items
                  }
                , { newDestinationAttempt | items = newDestinationAttempt.items |> List.Extra.removeAt newDestinationAttempt.capacity }
                )
            )


safeMoveSlotId : Int -> Container -> Container -> ( Container, Container )
safeMoveSlotId id source destination =
    moveSlotId id source destination
        |> Maybe.withDefault ( source, destination )


exampleContainer : Container
exampleContainer =
    { items = exampleItems
    , capacity = 24
    }


exampleContainer2 : Container
exampleContainer2 =
    { items =
        [ plantItem Corn 1
        , plantItem Wheat 32
        , plantItem Wheat 1
        , seedOf Corn 3
        , single (WateringCan 5)
        , single Hoe
        , single Hoe
        ]
    , capacity = 8
    }


checkNumbers =
    [ HoeKind
    , SeedKind Wheat
    , WheatKind
    , CornKind
    , WateringCanKind
    ]
        |> List.map (\i -> itemKindToString i ++ " = " ++ String.fromInt (itemCountInContainer i exampleContainer))
        |> String.join ", "
